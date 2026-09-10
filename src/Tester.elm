module Tester exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode
import ParsedExpo



-- This is a tester for JSON files.
-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Loading, fetchConfig )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type Model
    = Loading
    | Success ParsedExpo.ParsedExpo
    | Failure Http.Error



-- UPDATE


type Msg
    = GotExpo (Result Http.Error ParsedExpo.ParsedExpo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotExpo result ->
            case result of
                Ok expo ->
                    ( Success expo, Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )



-- HTTP


fetchConfig : Cmd Msg
fetchConfig =
    Http.get
        { url = "test1.json"
        , expect = Http.expectJson GotExpo ParsedExpo.decodeParsedExposition
        }



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model of
        Loading ->
            Html.text "Loading…"

        Success _ ->
            Html.text "It worked!"

        Failure error ->
            Html.text <| Debug.toString error 
