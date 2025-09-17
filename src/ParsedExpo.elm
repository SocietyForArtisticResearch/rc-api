module ParsedExpo exposing (..)

import AppUrl
import Dict exposing (Dict)
import Expo exposing (PageID, Tool, ToolId)
import Hyperlinks exposing (Hyperlinks)
import Iso8601
import Json.Decode as D
import Licenses exposing (License)
import Research
import Stats exposing (Stats)
import Time exposing (Posix, millisToPosix)
import Url exposing (Url)
 


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
    , tools : List Tool
    , weaveType : Expo.PageType
    }


type MediaResource
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
    , lastModified : Posix
    , lastModifiedBy : String
    , license : Licenses.License
    , name : String
    , src : MediaResource
    }


type ToolLink
    = ToolLink String


type alias Copyright =
    { copyrightHolder : String
    , id : ToolId
    , license : License
    , name : String
    , toolLink : ToolLink -- Can we link these two in the parser.
    , usages : List String
    }


usages =
    D.list D.string


toolId =
    D.int |> D.map Expo.ToolId


decodeCopyrights : D.Decoder Copyright
decodeCopyrights =
    D.map6 Copyright
        (D.field "copyright" D.string)
        (D.field "id" toolId)
        (D.field "license" (D.string |> D.map Licenses.fromString))
        (D.field "name" D.string)
        (D.field "toolLink" (D.string |> D.map ToolLink))
        (D.field "usages" usages)


decodeExpositionMeta : D.Decoder (Research.Research Research.Res)
decodeExpositionMeta =
    D.field "meta" Research.decoder


type alias PagesDict =
    Dict Expo.PageID Expo.Page


stringToIntId : ( String, Expo.Page ) -> ( Int, Expo.Page )
stringToIntId ( idStr, page ) =
    ( String.toInt idStr |> Maybe.withDefault -1, page )


decodePagesDict =
    D.keyValuePairs decodePage |> D.map (List.map stringToIntId)


decodePage =
    D.map5 ParsedPage
        (D.field "id" D.int)
        (D.field "hyperlinks" Hyperlinks.hyperlinksDecoder)
        (D.field "metrics" Stats.decodeStats)
        (D.field "tools" decodeTools)
        (D.field "type" decodePageType)


decodeTools =
    D.list decodeParsedTool


decodePageType =
    D.field "type" (D.string |> D.map Expo.pageTypeOfString)


decodeLastModified =
    D.oneOf
        [ decodeModifiedDate |> D.map Just
        , D.null Nothing
        ]


decodeParsedTool =
    let
        field =
            D.field
    in
    field "content"
        D.string
        (field "copyright" D.string)
        (field "dimensions" (D.list D.int))
        (field "id" decodeToolId)
        (field "last-modified-at" decodeIsoDate)
        (field "last-modified-by" D.string)


decodeIsoDate : D.Decoder Time.Posix
decodeIsoDate =
    D.string
        |> D.andThen
            (\str ->
                case Iso8601.toTime str of
                    Ok psx ->
                        D.succeed Time.Posix

                    Err e ->
                        D.fail e
            )


decodePages : D.Decoder PagesDict
decodePages =
    D.field "pages" decodePagesDict
