module ExpoApi exposing (..)

import Expo exposing (ToolId)
import Json.Decode as D
import Licenses exposing (License)
import Research
import Dict exposing (Dict)
import Expo exposing (PageID)



-- Parser for API expositions, for example
--https://map.rcdata.org/rcjson/expo/2281597


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
stringToIntId (idStr,page) =
    (String.toInt idStr |> Maybe.withDefault -1 ,page)

decodePagesDict =
    D.keyValuePairs decodePage |> List.map stringToIntId |> Dict.fromList
 

decodePage = 
    D.succeed {}

decodePages : D.Decoder PagesDict
decodePages =
    D.field "pages" decodePagesDict 



