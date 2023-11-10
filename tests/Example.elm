module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P
import RichAbstract as R
import Test exposing (Test, describe, test)


testParser : String -> String
testParser input =
    let
        parsed =
            input
                |> P.run R.abstractParser
                |> (\s ->
                        let
                            _ =
                                Debug.log "parsed" s
                        in
                        s
                   )
                |> Result.map R.asString

        safe =
            case parsed of
                Err e ->
                    P.deadEndsToString e

                Ok str ->
                    str
    in
    safe


suite : Test
suite =
    describe "RichAbstractWithKeywords"
        [ test "abstract with keywords encoded as string"
            (\_ ->
                test1
                    |> testParser
                    |> Expect.equal
                        test1
            )
        , test "second test" (\_ -> test2 |> testParser |> Expect.equal test2)
        ]


test1 =
    "{{kw}}{notkw}notkw{{kw2}}{notkw}}{}{notkw}}}{{{kw3}}"


test2 =
    "a simple example of a {{kw}} and another {{kw2}}"
