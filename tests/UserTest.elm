module UserTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import User


testUserJson : String
testUserJson =
    """[{"id":64680,"name":"Geir Strøm","orcid":null,"type":"user","description":"","keywords":[]}]"""


parseJson : Result String (List User.ApiUser)
parseJson =
    User.decodeString testUserJson


isOk result =
    case result of
        Ok _ ->
            True

        Err e ->
            let _ = Debug.log "test failed" e in
            False


testUser =
    describe
        "usertest"
        [ test "test parsing user"
            (\_ ->
                parseJson
                    |> isOk
                    |> Expect.equal
                        True
            )
        ]
