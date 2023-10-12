module Screenshots exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, andThen, keyValuePairs, list, map, string, succeed)
import Json.Encode
import Research exposing (ExpositionID)


type RCScreenshots
    = RCScreenshots (Dict String Exposition)


type Screenshot
    = Screenshot String


type Weave
    = Weave (List Screenshot)


type Exposition
    = Exposition (Dict String Weave)


encodeScreenshot : Screenshot -> Json.Encode.Value
encodeScreenshot (Screenshot url) =
    Json.Encode.string url


encodeExposition : Exposition -> Json.Encode.Value
encodeExposition (Exposition dict) =
    let
        encodeWeave (Weave ws) =
            ws |> Json.Encode.list encodeScreenshot
    in
    dict
        |> Dict.toList
        |> List.map (\( k, weave ) -> ( k, encodeWeave weave ))
        |> Json.Encode.object


flatList : Exposition -> List ( String, Screenshot )
flatList (Exposition d) =
    d
        |> Dict.toList
        |> List.map (\( k, Weave weaves ) -> weaves |> List.map (\w -> ( k, w )))
        |> List.concat


getScreenshots : ExpositionID -> RCScreenshots -> Maybe Exposition
getScreenshots id (RCScreenshots dict) =
    dict |> Dict.get (String.fromInt id)


decodeScreenshot : Json.Decode.Decoder Screenshot
decodeScreenshot =
    map Screenshot string


decodeWeave : Json.Decode.Decoder Weave
decodeWeave =
    map Weave (list decodeScreenshot)


decodeExposition : Json.Decode.Decoder Exposition
decodeExposition =
    let
        parser : Json.Decode.Decoder (List ( String, Weave ))
        parser =
            keyValuePairs decodeWeave
    in
    parser
        |> andThen
            (\lst ->
                Json.Decode.succeed (Exposition (Dict.fromList lst))
            )


decodeAll : Decoder RCScreenshots
decodeAll =
    keyValuePairs decodeExposition
        |> andThen
            (\lst ->
                succeed (RCScreenshots (lst |> Dict.fromList))
            )
