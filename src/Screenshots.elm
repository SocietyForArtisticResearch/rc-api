module Screenshots exposing
    ( Exposition(..)
    , RCScreenshots(..)
    , Screenshot(..)
    , Weave(..)
    , WeaveScreenshot
    , decodeAll
    , decodeExposition
    , encodeExposition
    , getScreenshots
    , getUrls
    , getWeaveAndScreenshot
    )

import Dict exposing (Dict)
import Json.Decode exposing (andThen, keyValuePairs, list, map, string)
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


getScreenshots : ExpositionID -> RCScreenshots -> Maybe Exposition
getScreenshots id (RCScreenshots dict) =
    dict |> Dict.get (String.fromInt id)


flatList : Exposition -> List ( String, Screenshot )
flatList (Exposition d) =
    d
        |> Dict.toList
        |> List.concatMap (\( k, Weave weaves ) -> weaves |> List.map (\w -> ( k, w )))


getUrls : String -> ExpositionID -> Exposition -> List String
getUrls baseUrl expoId exp =
    exp
        |> flatList
        |> List.map
            (\( key, Screenshot png ) ->
                String.join "/" [ baseUrl, expoId |> String.fromInt, key, png ]
            )


type alias WeaveScreenshot =
    { weave : String
    , screenshot : String
    }


getWeaveAndScreenshot : String -> ExpositionID -> Exposition -> List WeaveScreenshot
getWeaveAndScreenshot baseUrl expoId exp =
    exp
        |> flatList
        |> List.map
            (\( key, Screenshot png ) ->
                WeaveScreenshot
                    ("https://www.researchcatalogue.net/view/" ++ String.fromInt expoId ++ "/" ++ key)
                    (String.join "/" [ baseUrl, expoId |> String.fromInt, key, png ])
            )


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


decodeAll : Json.Decode.Decoder RCScreenshots
decodeAll =
    keyValuePairs decodeExposition
        |> andThen
            (\lst ->
                Json.Decode.succeed (RCScreenshots (lst |> Dict.fromList))
            )
