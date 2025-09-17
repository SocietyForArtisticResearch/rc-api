module Hyperlinks exposing (..)

import Json.Decode as D


type alias Hyperlinks =
    { external : List String
    , other_expositions : List String
    , references : List String
    , same_exposition : List String
    , simpleurls : List String
    }



-- plain text urls


stringList : D.Decoder (List String)
stringList =
    D.list D.string


hyperlinksDecoder : D.Decoder Hyperlinks
hyperlinksDecoder =
    D.map5 Hyperlinks
        (D.field "external" stringList)
        (D.field "other_expositions" stringList)
        (D.field "references" stringList)
        (D.field "same_exposition" stringList)
        (D.field "simpleurls" stringList)
