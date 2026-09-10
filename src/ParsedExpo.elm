module ParsedExpo exposing (..)

import AppUrl
import Dict exposing (Dict)
import Expo exposing (Dimensions, PageID, Tool, ToolId)
import Hyperlinks exposing (Hyperlinks)
import Iso8601
import Json.Decode as D exposing (field)
import Json.Decode.Pipeline exposing (optional, required)
import Licenses exposing (License)
import Parser
import Research
import Stats exposing (Stats)
import Time exposing (Posix, millisToPosix)
import Toc exposing (decode)
import Tools
import Url exposing (Url)



-- Parser for API expositions, for example
--https://map.rcdata.org/rcjson/expo/2281597


type alias ParsedExpo =
    { meta : Research.Research Research.Res
    , id : Research.ExpositionID
    , copyrights : List Copyright
    , pages : List ParsedPage
    }


type alias ParsedPage =
    { pageId : Int
    , pageHyperlinks : Hyperlinks
    , tools : List ( String, List ParsedTool )
    , weaveType : Expo.PageType
    , metrics : Stats.Metrics
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
                appurl : AppUrl.AppUrl
                appurl =
                    url |> AppUrl.fromUrl

                string2posix : String -> Maybe Posix
                string2posix secondsString =
                    secondsString |> String.toInt |> Maybe.map Time.millisToPosix

                end : Maybe Posix
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
    ,  copyright : Maybe String
    , dimensions : Dimensions
    , lastModified : Maybe Posix
    , lastModifiedBy : Maybe String
    , license : Maybe Licenses.License
    , name : String
    , src : Maybe MediaResource
    , style : String
    , tool : String -- this is just the raw source
    , usages : List String
    }


type alias Copyright =
    { copyrightHolder : String
    , id : List ToolId
    , license : License
    , name : String
    , toolLink : List Url.Url -- Can we link these two in the parser.
    , usages : List String
    }


toolIds =
    D.list D.string
        |> D.map
            (\lst ->
                let
                    maybeTool =
                        String.replace "tool-" ""
                            >> String.toInt
                            >> Maybe.map Expo.ToolId
                in
                lst |> List.filterMap maybeTool
            )


toolUrls =
    D.list D.string
        |> D.map (List.filterMap Url.fromString)


usages =
    D.oneOf
        [ D.string |> D.map (\x -> [ x ])
        , D.list D.string
        ]


decodeCopyrights : D.Decoder Copyright
decodeCopyrights =
    D.map6 Copyright
        (field "copyright" D.string)
        (field "id" toolIds)
        (field "license" (D.string |> D.map Licenses.fromString))
        (field "name" D.string)
        (field "tool" toolUrls)
        (field "usages" usages)


decodeExpositionMeta : D.Decoder (Research.Research Research.Res)
decodeExpositionMeta =
    Research.decoder


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
        |> optional "copyright" (D.map Just D.string) Nothing
        |> required "dimensions" decodeParsedDimensions
        |> optional "last-modified-at" (D.map Just decodeIsoDate) Nothing
        |> optional "last-modified-by" (D.map Just D.string) Nothing
        |> optional "license" (D.map (Licenses.fromString >> Just) D.string) Nothing
        |> optional "name" D.string ""
        |> optional "src" (D.string |> D.map mkMediaResource) Nothing
        |> required "style" D.string
        |> required "tool" D.string
        |> optional "usages" usages []


decodeToolsDict : D.Decoder (List ( String, List ParsedTool ))
decodeToolsDict =
    D.keyValuePairs (D.list decodeParsedTool)


decodePage : D.Decoder ParsedPage
decodePage =
    D.map5 ParsedPage
        (field "id" D.int)
        (field "hyperlinks" Hyperlinks.hyperlinksDecoder)
        (field "tools" decodeToolsDict)
        (field "type" decodePageType)
        (field "metrics" Stats.decodeMetrics)


decodeToolsOfType : D.Decoder (List ParsedTool)
decodeToolsOfType =
    D.list decodeParsedTool


decodePageType : D.Decoder Expo.PageType
decodePageType =
    (D.string |> D.map Expo.pageTypeOfString)


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
    D.string
        |> D.andThen
            (\str ->
                case Url.fromString str of
                    Nothing ->
                        D.fail ("incorrect tool url: ***" ++ str ++ "***")

                    Just url ->
                        D.succeed url
            )


decodeParsedExposition : D.Decoder ParsedExpo
decodeParsedExposition =
    D.map4 ParsedExpo
        (field "meta" decodeExpositionMeta)
        (field "id" D.int)
        (field "copyrights" (D.list decodeCopyrights))
        (field "pages" decodePages)


decodeExpoFromJsonString : String -> Result D.Error ParsedExpo
decodeExpoFromJsonString str =
    D.decodeString decodeParsedExposition str
