module RichAbstract exposing
    ( AbstractSpan(..)
    , AbstractWithKeywords
    , abstractParser
    , asString
    ,  decodeAbstract
       --    , decodeAbstractSpan

    , decodeAbstractWithKeywords
    , encodeAbstract
    ,  encodeAbstract2
       --  , encodeAbstractSpan

    , parseKeyword
    )

import Json.Decode as D
import Json.Encode
import Parser as P exposing ((|.), (|=), Parser, Step(..))


type alias AbstractWithKeywords =
    List AbstractSpan


type AbstractSpan
    = AbsKw String -- A keyword
    | AbsText String -- Nomal text



-- encodeAbstractSpan2 : AbstractSpan -> Json.Encode.Value
-- encodeAbstractSpan2 span =
--     case span of
--         AbsKw s ->
--             Json.Encode.object
--                 [ ( "t", Json.Encode.string "AbsKw" )
--                 , ( "s", Json.Encode.string s )
--                 ]
--         AbsText s ->
--             Json.Encode.object
--                 [ ( "t", Json.Encode.string "AbsText" )
--                 , ( "s", Json.Encode.string s )
--                 ]
-- decodeAbstractSpan : D.Decoder AbstractSpan
-- decodeAbstractSpan =
--     D.field "t" D.string
--         |> D.andThen
--             (\t ->
--                 case t of
--                     "AbsKw" ->
--                         D.map AbsKw
--                             (D.field "s" D.string)
--                     "AbsText" ->
--                         D.map AbsText
--                             (D.field "s" D.string)
--                     _ ->
--                         D.fail "abstract decoder expected a AbsKw or AbsText"
--             )


encodeAbstract : AbstractWithKeywords -> Json.Encode.Value
encodeAbstract abstract =
    Json.Encode.string (asString abstract)


decodeAbstractWithKeywords : D.Decoder AbstractWithKeywords
decodeAbstractWithKeywords =
    decodeAbstract


chompExactlyFast : Int -> (Char -> Bool) -> Parser ()
chompExactlyFast n isGood =
    P.loop n
        (\i ->
            if i <= 0 then
                P.succeed <| P.Done ()

            else
                P.chompIf isGood
                    |> P.andThen (\_ -> P.succeed (P.Loop (i - 1)))
        )


parseOrEnd : () -> Parser ()
parseOrEnd () =
    P.succeed ()
        |. P.chompIf (\c -> c /= '}')
        |. P.chompWhile (\c -> c /= '}')
        |. P.oneOf
            [ chompExactlyFast 2 (\c -> c == '}')
            , P.end
            , P.lazy parseOrEnd
            ]


parseKeyword : Parser String
parseKeyword =
    P.succeed (\str -> String.slice 0 -2 str)
        |. P.token "{{"
        |= P.oneOf
            [ P.keyword "}}"
                |> P.map (always "")
            , P.getChompedString
                (parseOrEnd ())
            ]


notCurly c =
    c /= '{'


isCurly c =
    c == '{'


parseUntilKeyword : Parser String
parseUntilKeyword =
    P.getChompedString
        (P.succeed ()
            |. P.chompIf notCurly
            |. P.chompWhile notCurly
        )



-- singleCurly : Parser String
-- singleCurly =
--     P.getChompedString
--         (P.succeed ()
--             |. P.chompIf isCurly
--             |. P.chompIf notCurly
--         )


theUnhappyCase : Parser AbstractSpan
theUnhappyCase =
    let
        secondCurly : Parser AbstractSpan
        secondCurly =
            P.succeed identity
                |. P.symbol "{"
                |= (P.getChompedString (parseOrEnd ()) |> P.map AbsKw)

        noSecond : Parser AbstractSpan
        noSecond =
            parseUntilKeyword |> P.map (\rest -> AbsText ("{" ++ rest))
    in
    P.succeed identity
        |. P.symbol "{"
        |= P.oneOf
            [ P.end |> P.map (always (AbsText "{"))
            , secondCurly
            , noSecond
            ]



-- debugParse : String -> Parser a -> Parser a
-- debugParse help p =
--     p
--         |> P.andThen
--             (\r ->
--                 let
--                     _ =
--                         Debug.log help p
--                 in
--                 P.succeed r
--             )


parseBoth : Parser AbstractSpan
parseBoth =
    P.oneOf
        [ parseKeyword |> P.map AbsKw
        , theUnhappyCase
        , parseUntilKeyword |> P.map AbsText
        ]


abstractParser : Parser AbstractWithKeywords
abstractParser =
    let
        helper lst =
            P.oneOf
                [ parseBoth |> P.map (\segment -> Loop (segment :: lst))
                , P.succeed (Done (List.reverse lst))
                ]
    in
    P.loop [] helper


decodeAbstract : D.Decoder AbstractWithKeywords
decodeAbstract =
    D.string
        |> D.andThen
            (\str ->
                case str |> P.run abstractParser of
                    Ok abs ->
                        D.succeed abs

                    Err e ->
                        D.fail ("abstract with keywords could not be parsed: " ++ P.deadEndsToString e)
            )


encodeSpanAsString : AbstractSpan -> String
encodeSpanAsString span =
    case span of
        AbsKw kw ->
            "{{" ++ kw ++ "}}"

        AbsText txt ->
            txt


asString : AbstractWithKeywords -> String
asString abstract =
    abstract
        |> List.map encodeSpanAsString
        |> String.concat


encodeAbstract2 : AbstractWithKeywords -> Json.Encode.Value
encodeAbstract2 abstract =
    abstract
        |> asString
        |> Json.Encode.string
