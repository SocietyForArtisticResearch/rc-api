module Toc exposing (Dimensions, ExpositionToc, Id, Weave, decode, decodeToc, encodeToc)

import Json.Decode as Decode
import Json.Decode.Extra exposing (andMap)
import Json.Encode


type alias Id =
    Int


type alias ExpositionToc =
    { expoId : Id
    , weaves : List Weave
    }


expositionToc : Id -> List Weave -> ExpositionToc
expositionToc id lst =
    { expoId = id
    , weaves = lst
    }


encodeToc : ExpositionToc -> Json.Encode.Value
encodeToc expToc =
    Json.Encode.object
        [ ( "expoId", Json.Encode.int expToc.expoId )
        , ( "weaves", Json.Encode.list encodeWeave expToc.weaves )
        ]


decodeToc : Decode.Decoder ExpositionToc
decodeToc =
    Decode.map2 expositionToc
        (Decode.field "expoId" Decode.int)
        (Decode.field "weaves" (Decode.list decodeWeave))


type alias Weave =
    { file : String
    , page : Int
    , pageTitle : String
    , url : String
    , weaveSize : Dimensions
    }


type alias Dimensions =
    { height : Int
    , width : Int
    }


pageWithWeaves : ( String, List Weave ) -> ExpositionToc
pageWithWeaves ( idStr, weaves ) =
    { expoId = Maybe.withDefault 0 (String.toInt idStr)
    , weaves = weaves
    }



-- Decoders


decode : Decode.Decoder (List ExpositionToc)
decode =
    let
        obj : Decode.Decoder (List ( String, List Weave ))
        obj =
            Decode.keyValuePairs (Decode.list decodeWeave)
    in
    -- inverse monad, first concat than map ?
    obj
        |> Decode.map (List.map pageWithWeaves)


decodeWeave : Decode.Decoder Weave
decodeWeave =
    Decode.succeed Weave
        |> andMap (Decode.field "file" Decode.string)
        |> andMap (Decode.field "page" (Decode.string |> Decode.map (String.toInt >> Maybe.withDefault 0)))
        |> andMap (Decode.field "page_title" Decode.string)
        |> andMap (Decode.field "url" Decode.string)
        |> andMap (Decode.field "weave_size" decodeDimensions)


decodeDimensions : Decode.Decoder Dimensions
decodeDimensions =
    Decode.oneOf
        [ Decode.succeed Dimensions
            |> andMap (Decode.field "height" Decode.int)
            |> andMap (Decode.field "width" Decode.int)
        , Decode.succeed { width = 1920, height = 1440 }
        ]



-- Automatically generated encoders (may be needed later)


encodeWeave : Weave -> Json.Encode.Value
encodeWeave weave =
    Json.Encode.object
        [ ( "file", Json.Encode.string weave.file )
        , ( "page", Json.Encode.int weave.page )
        , ( "page_title", Json.Encode.string weave.pageTitle )
        , ( "url", Json.Encode.string weave.url )
        , ( "weave_size", encodeDimensions weave.weaveSize )
        ]


encodeDimensions : Dimensions -> Json.Encode.Value
encodeDimensions d =
    Json.Encode.object
        [ ( "height", Json.Encode.int d.height )
        , ( "width", Json.Encode.int d.width )
        ]
