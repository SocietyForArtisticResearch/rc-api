module Licenses exposing (License, fromString)


type License
    = AllRightsReserved
    | PublicDomain
    | CCBY
    | CCBYNC
    | CCBYNCND
    | CCBYNCSA


fromString : String -> License
fromString str =
    case str of
        "all-rights-reserved" ->
            AllRightsReserved

        "cc-by" ->
            CCBY

        "cc-by-nc" ->
            CCBYNC

        "cc-by-nc-nd" ->
            CCBYNCND

        "cc-by-nc-sa" ->
            CCBYNCSA

        "public-domain" ->
            PublicDomain

        _ ->
            AllRightsReserved
