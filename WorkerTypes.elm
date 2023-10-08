module WorkerTypes exposing (..)

import Date exposing (Date)
import EnrichedResearch exposing (ResearchWithKeywords)
import Json.Decode exposing (Decoder)
import Json.Decode.Extra as JDE
import Json.Encode exposing (Value)
import KeywordString exposing (KeywordString)
import Research as RC exposing (Author, Portal, PublicationStatus(..), Research)
import Toc



{-
   In Research.elm , the decoders sometimes have to do some calculation to
   retrieve a desired value from the RC API. However, once we have a nice elm type, we can use this
   to communicate to the Worker.
   So, these types and json encoders/decoders only exist to communicate between main elm.js and the worker.js
-}


decodeExposition : Decoder (RC.Research ResearchWithKeywords)
decodeExposition =
    let
        field =
            Json.Decode.field

        int =
            Json.Decode.int

        string =
            Json.Decode.string

        andMap =
            JDE.andMap
    in
    field "type" string
        |> Json.Decode.andThen
            (\typ ->
                case typ of
                    "exposition" ->
                        Json.Decode.map EnrichedResearch.mkResearchWithKeywords
                            (field "id" int)
                            |> andMap (field "title" string)
                            |> andMap (field "keywords" (Json.Decode.list string) |> Json.Decode.map (List.map KeywordString.fromString))
                            |> andMap (field "created" string)
                            |> andMap (field "author" RC.author)
                            |> andMap (JDE.optionalField "issueId" int)
                            |> andMap (field "publicationStatus" RC.decodePublicationStatus)
                            |> andMap (JDE.optionalField "publication" (Json.Decode.int |> Json.Decode.map Date.fromRataDie))
                            |> andMap (JDE.optionalField "thumbnail" string)
                            |> andMap (JDE.optionalField "abstract" string)
                            |> andMap (field "defaultPage" string)
                            |> andMap (field "portals" (Json.Decode.list RC.decodePortal))
                            |> andMap (field "abstractWithKeywords" EnrichedResearch.decodeAbstractWithKeywords)
                            |> andMap (Json.Decode.field "toc" (Json.Decode.maybe Toc.decodeToc))

                    _ ->
                        Json.Decode.fail "expected an exposition"
            )


encodeExposition : Research r -> Json.Encode.Value
encodeExposition exp =
    let
        int =
            Json.Encode.int

        string =
            Json.Encode.string

        list =
            Json.Encode.list

        maybeAppend x xs =
            case x of
                Just v ->
                    v :: xs

                Nothing ->
                    xs

        issueId =
            exp.issueId
                |> Maybe.map
                    (\id ->
                        ( "id", int id )
                    )

        publication =
            exp.publication
                |> Maybe.map
                    (\p ->
                        ( "publication", int (Date.toRataDie p) )
                    )

        thumbnail =
            exp.thumbnail
                |> Maybe.map
                    (\t ->
                        ( "thumbnail", string t )
                    )

        abstract =
            exp.abstract
                |> Maybe.map
                    (\a ->
                        ( "abstract", string a )
                    )
    in
    Json.Encode.object
        ([ ( "type", string "exposition" )
         , ( "id", int exp.id )
         , ( "created", string exp.created )
         , ( "title", string exp.title )
         , ( "keywords", list string (List.map KeywordString.toString exp.keywords) )
         , ( "author", RC.encodeAuthor exp.author )
         , ( "publicationStatus", RC.publicationstatus exp.publicationStatus )
         , ( "defaultPage", string exp.defaultPage )
         , ( "portals", list RC.encodePortal exp.portals )
         ]
            |> maybeAppend issueId
            |> maybeAppend publication
            |> maybeAppend thumbnail
            |> maybeAppend abstract
        )
