module ParsedExpo exposing (..)

import AppUrl
import Dict exposing (Dict)
import Expo exposing (Dimensions, PageID, Tool, ToolId)
import Hyperlinks exposing (Hyperlinks)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Licenses exposing (License)
import Parser
import Research
import Stats exposing (Stats)
import Time exposing (Posix, millisToPosix)
import Url exposing (Url)
import Toc exposing (decode)



-- Parser for API expositions, for example
--https://map.rcdata.org/rcjson/expo/2281597


type alias ParsedExpo =
    { meta : Research.Research Research.Res
    , metrics : Stats.Stats
    , id : Research.ExpositionID
    , copyrights : List Copyright
    , pages : List ParsedPage
    }


type alias ParsedPage =
    { pageId : Int
    , pageHyperlinks : Hyperlinks
    , tools : List ParsedTool
    , weaveType : Expo.PageType
    , stats : Stats.Stats
    }


type
    MediaResource
    -- This is a link
    = MediaResource
        { src : Url.Url
        , timeout : Maybe Posix
        }


mkMediaResource : String -> Maybe MediaResource
mkMediaResource urlString =
    let
        -- we can track the timeout, to make sure we have a url that actually works.
        -- "https://media.researchcatalogue.net/rc/cache/cc/66/20/da/cc6620daa90f0a6d4dae7104150187b5.png?t=3ac1bd0550be0facf3834de328af84f1&e=1757112300"
        fromUrl url =
            let
                appurl =
                    url |> AppUrl.fromUrl

                string2posix secondsString =
                    secondsString |> String.toInt |> Maybe.map Time.millisToPosix

                end =
                    appurl.queryParameters |> Dict.get "e" |> Maybe.andThen List.head |> Maybe.andThen string2posix
            in
            MediaResource
                { src = url
                , timeout = end
                }
    in
    Url.fromString urlString
        |> Maybe.map
            fromUrl


type alias ParsedTool =
    { id : String
    , content : String
    , copyright : String
    , dimensions : Dimensions
    , lastModified : Posix
    , lastModifiedBy : String
    , license : Licenses.License
    , name : String
    , src : Maybe MediaResource
    , style : String
    , tool : Url.Url
    , usages : List String
    }


type alias Copyright =
    { copyrightHolder : String
    , id : ToolId
    , license : License
    , name : String
    , toolLink : Url.Url -- Can we link these two in the parser.
    , usages : List String
    }


usages =
    D.list D.string


toolId =
    D.int |> D.map Expo.ToolId


toolUrl =
    D.string
        |> D.andThen
            (\str ->
                case Url.fromString str of
                    Nothing ->
                        D.fail "incorrect tool url detected"

                    Just url ->
                        D.succeed url
            )


decodeCopyrights : D.Decoder Copyright
decodeCopyrights =
    D.map6 Copyright
        (D.field "copyright" D.string)
        (D.field "id" toolId)
        (D.field "license" (D.string |> D.map Licenses.fromString))
        (D.field "name" D.string)
        (D.field "toolLink" toolUrl)
        (D.field "usages" (D.list D.string))


decodeExpositionMeta : D.Decoder (Research.Research Research.Res)
decodeExpositionMeta =
    D.field "meta" Research.decoder


type alias PagesDict =
    Dict Expo.PageID Expo.Page


stringToIntId : ( String, Expo.Page ) -> ( Int, Expo.Page )
stringToIntId ( idStr, page ) =
    ( String.toInt idStr |> Maybe.withDefault -1, page )


decodePages : D.Decoder (List ParsedPage)
decodePages =
    D.keyValuePairs decodePage |> D.map (List.map Tuple.second)


decodeParsedTool : D.Decoder ParsedTool
decodeParsedTool =
    D.succeed ParsedTool
        |> required "id" D.string
        |> required "content" D.string
        |> required "copyright" D.string
        |> required "dimensions" decodeParsedDimensions
        |> required "last-modified-at" decodeIsoDate
        |> required "last-modified-by" D.string
        |> required "license" (D.map Licenses.fromString D.string)
        |> required "name" D.string
        |> required "source" (D.string |> D.map mkMediaResource)
        |> required "style" D.string
        |> required "tool" decodeToolLink
        |> required "usages" usages


decodeToolsDict : D.Decoder (List ParsedTool)
decodeToolsDict =
    D.keyValuePairs decodeParsedTool |> D.map (List.map Tuple.second)

decodePage : D.Decoder ParsedPage
decodePage =
    D.map5 ParsedPage
        (D.field "id" D.int)
        (D.field "hyperlinks" Hyperlinks.hyperlinksDecoder)
        (D.field "tools" decodeToolsDict)
        (D.field "type" decodePageType)
        (D.field "metrics" Stats.decodeStats)


decodeTools =
    D.list decodeParsedTool


decodePageType =
    D.field "type" (D.string |> D.map Expo.pageTypeOfString)


decodeIsoDate : D.Decoder Time.Posix
decodeIsoDate =
    D.string
        |> D.andThen
            (\str ->
                case Iso8601.toTime str of
                    Ok psx ->
                        D.succeed psx

                    Err e ->
                        D.fail ("error: " ++ Parser.deadEndsToString e)
            )


decodeLastModified =
    D.oneOf
        [ decodeIsoDate |> D.map Just
        , D.null Nothing
        ]


decodeParsedDimensions =
    let
        dimensionsFromList lst =
            case lst of
                [ left, top, width, height ] ->
                    -- place encoded in the API !
                    D.succeed (Expo.CartDim { left = left, top = top, w = width, h = height })

                wrongLst ->
                    D.fail ("invalid position format" ++ (wrongLst |> List.map String.fromInt |> String.join " "))
    in
    D.list D.int |> D.andThen dimensionsFromList

decodeToolLink =
    D.string |> D.andThen (\str -> 
        case Url.fromString str of
            Nothing -> D.fail "incorrect tool url"

            Just url -> 
                D.succeed url)

