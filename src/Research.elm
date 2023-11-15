module Research exposing
    ( Author(..)
    , Compare(..)
    , ExpositionID
    , Keyword(..)
    , KeywordSet(..)
    , KeywordSorting(..)
    , Portal
    , PortalType(..)
    , PublicationStatus(..)
    , Research
    , ReverseKeywordDict
    , TitleSorting(..)
    , author
    , authorAsString
    , authorUrl
    , calcStatus
    , dateFromRCString
    , decodeKeyword
    , decodePortal
    , decoder
    , dmyToYmd
    , emptyKeywordSet
    , encodeAuthor
    , encodeKeyword
    , encodePortal
    , encodeSet
    , findExpositionWithId
    , findResearchAfter
    , findResearchBefore
    , findResearchWithAbstract
    , findResearchWithAuthor
    , findResearchWithKeywords
    , findResearchWithPortal
    , findResearchWithTitle
    , getAllPortals
    , getCount
    , getName
    , insert
    , keywordSet
    , kwName
    , publicationstatus
    , rcDateToPosix
    , rcDateToRataDie
    , rcPortalDecoder
    , reverseKeywordDict
    , shuffleWithSeed
    , sortingFromString
    , sortingToString
    , titleSortingFromString
    , titleSortingToString
    , toList
    )

import Date exposing (Date)
import Dict exposing (Dict)
import Iso8601
import Json.Decode exposing (Decoder, field, int, maybe, string)
import Json.Decode.Extra as JDE
import Json.Encode
import KeywordString exposing (KeywordString)
import List.Extra exposing (uniqueBy)
import Random
import Random.List
import Set exposing (Set)
import Time


type alias ExpositionID =
    Int


type alias Portal =
    { id : Int
    , name : String
    , type_ : PortalType
    }


encodePortal : Portal -> Json.Encode.Value
encodePortal portal =
    Json.Encode.object
        [ ( "id", Json.Encode.int portal.id )
        , ( "name", Json.Encode.string portal.name )
        , ( "type_", Json.Encode.string (portal.type_ |> portalTypeToString) )
        ]


decodePortal : Decoder Portal
decodePortal =
    Json.Decode.map3 Portal
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "type_" Json.Decode.string |> Json.Decode.map portalTypeFromString)


type PortalType
    = Institutional
    | Journal
    | Project
    | MainPortal


portalTypeToString : PortalType -> String
portalTypeToString portaltype =
    case portaltype of
        Institutional ->
            "Institutional"

        Journal ->
            "Journal"

        Project ->
            "Project"

        MainPortal ->
            "MainPortal"


portalTypeFromString : String -> PortalType
portalTypeFromString str =
    case str of
        "Institutional" ->
            Institutional

        "Journal" ->
            Journal

        "Project" ->
            Project

        "MainPortal" ->
            MainPortal

        _ ->
            Institutional



-- RC API portal lookup:


portalType : String -> PortalType
portalType portalName =
    let
        institutional =
            [ "KC Research Portal", "Stockholm University of the Arts (SKH)", "University of the Arts Helsinki", "Norwegian Academy of Music", "The Danish National School of Performing Arts", "Rhythmic Music Conservatory Copenhagen", "Konstfack - University of Arts, Crafts and Design", "NTNU", "i2ADS - Research Institute in Art, Design and Society", "University of Applied Arts Vienna", "Academy of Creative and Performing Arts", "International Center for Knowledge in the Arts (Denmark)", "Inland Norway University of Applied Sciences, The Norwegian Film School", "Fontys Academy of the Arts (internal)" ]
    in
    -- TODO match  for other types of portal !
    if List.member portalName institutional then
        Institutional

    else
        Journal



-- RC API


rcPortalDecoder : Json.Decode.Decoder Portal
rcPortalDecoder =
    Json.Decode.map3 Portal
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string |> Json.Decode.map portalType)


type Author
    = Author { id : Int, name : String }


authorAsString : Author -> String
authorAsString (Author a) =
    a.name


getAuthorId : Author -> Int
getAuthorId (Author a) =
    a.id


authorUrl : Author -> String
authorUrl (Author a) =
    "https://www.researchcatalogue.net/profile/?person=" ++ String.fromInt a.id


type PublicationStatus
    = InProgress
    | Published
    | Undecided



-- RC API


publicationstatus : PublicationStatus -> Json.Encode.Value
publicationstatus status =
    Json.Encode.string
        (case status of
            InProgress ->
                "inprogress"

            Published ->
                "published"

            Undecided ->
                "undecided"
        )



-- RC API


decodePublicationStatus : Json.Decode.Decoder PublicationStatus
decodePublicationStatus =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                Json.Decode.succeed
                    (case str of
                        "inprogress" ->
                            InProgress

                        "published" ->
                            Published

                        _ ->
                            Undecided
                    )
            )


type KeywordSet
    = KeywordSet
        { dict : Dict String Keyword
        , list : List Keyword
        }


encodeSet : KeywordSet -> Json.Encode.Value
encodeSet (KeywordSet d) =
    d.dict |> Dict.values |> Json.Encode.list encodeKeyword


type KeywordSorting
    = ByUse
    | Alphabetical
    | RandomKeyword


sortingToString : KeywordSorting -> String
sortingToString s =
    case s of
        ByUse ->
            "byuse"

        Alphabetical ->
            "alphabetical"

        RandomKeyword ->
            "randomkeyword"


sortingFromString : String -> KeywordSorting
sortingFromString str =
    case str of
        "byuse" ->
            ByUse

        "alphabetical" ->
            Alphabetical

        "randomkeyword" ->
            RandomKeyword

        _ ->
            ByUse


type TitleSorting
    = Random
    | OldestFirst
    | NewestFirst


titleSortingFromString : String -> TitleSorting
titleSortingFromString string =
    case string of
        "random" ->
            Random

        "oldestfirst" ->
            OldestFirst

        "newestfirst" ->
            NewestFirst

        _ ->
            NewestFirst


titleSortingToString : TitleSorting -> String
titleSortingToString sorting =
    case sorting of
        Random ->
            "random"

        OldestFirst ->
            "oldestfirst"

        NewestFirst ->
            "newestfirst"


type Keyword
    = Keyword { count : Int, name : String }


keyword : Int -> String -> Keyword
keyword count name =
    Keyword { count = count, name = name }


encodeKeyword : Keyword -> Json.Encode.Value
encodeKeyword (Keyword kw) =
    Json.Encode.object
        [ ( "type", Json.Encode.string "keyword" )
        , ( "count", Json.Encode.int kw.count )
        , ( "name", Json.Encode.string kw.name )
        ]


getAllPortals : List (Research r) -> List Portal
getAllPortals lst =
    lst
        |> List.concatMap .portals
        |> uniqueBy (\p -> p.id)


decodeKeyword : Json.Decode.Decoder Keyword
decodeKeyword =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\typ ->
                case typ of
                    "keyword" ->
                        Json.Decode.map2 keyword
                            (Json.Decode.field "count" Json.Decode.int)
                            (Json.Decode.field "name" Json.Decode.string)

                    _ ->
                        Json.Decode.fail "this is not a keyword"
            )


keywordSet : List (Research r) -> KeywordSet
keywordSet researchlist =
    List.foldr
        (\research set ->
            List.foldr insert set research.keywords
        )
        emptyKeywordSet
        researchlist


toList : KeywordSet -> List Keyword
toList (KeywordSet kwSet) =
    kwSet.list


emptyKeywordSet : KeywordSet
emptyKeywordSet =
    KeywordSet { dict = Dict.empty, list = [] }


use : Keyword -> Keyword
use (Keyword kw) =
    Keyword { kw | count = kw.count + 1 }


newKey : String -> Keyword
newKey str =
    Keyword { count = 1, name = str |> String.toLower }


insert : KeywordString -> KeywordSet -> KeywordSet
insert k (KeywordSet set) =
    let
        klower =
            KeywordString.toString k

        dict : Dict String Keyword
        dict =
            set.dict

        result : Maybe Keyword
        result =
            Dict.get klower dict
    in
    case result of
        Just (Keyword kw) ->
            let
                used : Keyword
                used =
                    use (Keyword kw)

                newDict : Dict String Keyword
                newDict =
                    Dict.insert (kw.name |> String.toLower) used dict
            in
            KeywordSet { set | dict = newDict, list = Dict.values newDict }

        Nothing ->
            let
                new : Keyword
                new =
                    newKey klower
            in
            KeywordSet { set | dict = Dict.insert klower new dict, list = new :: set.list }


kwName : Keyword -> String
kwName (Keyword kw) =
    kw.name |> String.toLower


getCount : Keyword -> Int
getCount (Keyword kw) =
    kw.count


getName : Author -> String
getName (Author data) =
    data.name


shuffleWithSeed : Int -> List a -> List a
shuffleWithSeed seed lst =
    Random.initialSeed seed
        |> Random.step (Random.List.shuffle lst)
        |> Tuple.first



-- RC API


author : Decoder Author
author =
    let
        makeAuthor : Int -> String -> Author
        makeAuthor id name =
            Author { id = id, name = name }
    in
    Json.Decode.map2
        makeAuthor
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


encodeAuthor : Author -> Json.Encode.Value
encodeAuthor au =
    Json.Encode.object
        [ ( "name", Json.Encode.string (getName au) )
        , ( "id", Json.Encode.int (getAuthorId au) )
        ]


mkResearch : ExpositionID -> String -> List KeywordString -> String -> Author -> Maybe Int -> PublicationStatus -> Maybe Date -> Maybe String -> Maybe String -> String -> List Portal -> Res
mkResearch e t kw cr au iss pubstat pub thumb abs def portals =
    { id = e
    , title = t
    , keywords = kw
    , created = cr
    , author = au
    , issueId = iss
    , publicationStatus = pubstat
    , publication = pub
    , thumbnail = thumb
    , abstract = abs
    , defaultPage = def
    , portals = portals
    }


{-| This is the RC API decoder. Data is retrieved by concatting the json output from the advanced search of the RC.
-}
decoder : Decoder (Research Res)
decoder =
    let
        researchPublicationStatus : Research Res -> Research Res
        researchPublicationStatus research =
            { research | publicationStatus = calcStatus research }

        statusFromString : String -> PublicationStatus
        statusFromString statusString =
            case statusString of
                "published" ->
                    Published

                "progress" ->
                    InProgress

                _ ->
                    Undecided
    in
    Json.Decode.map researchPublicationStatus <|
        (Json.Decode.succeed
            mkResearch
            |> JDE.andMap (field "id" int)
            |> JDE.andMap (field "title" string)
            |> JDE.andMap (field "keywords" (Json.Decode.list string) |> Json.Decode.map (List.map KeywordString.fromString))
            |> JDE.andMap (field "created" string |> Json.Decode.map dmyToYmd)
            |> JDE.andMap (field "author" author)
            |> JDE.andMap (maybe (field "issue" <| field "id" int))
            |> JDE.andMap (Json.Decode.map statusFromString (field "status" string))
            |> JDE.andMap (maybe (field "published" string) |> Json.Decode.map (Maybe.andThen dateFromRCString))
            |> JDE.andMap (maybe (field "thumb" string))
            |> JDE.andMap (maybe (field "abstract" string))
            |> JDE.andMap (field "default-page" string)
            |> JDE.andMap (field "published_in" (Json.Decode.list rcPortalDecoder))
        )


type alias Res =
    { id : ExpositionID
    , title : String
    , keywords : List KeywordString
    , created : String
    , author : Author
    , issueId : Maybe Int
    , publicationStatus : PublicationStatus -- should be string?
    , publication : Maybe Date
    , thumbnail : Maybe String
    , abstract : Maybe String
    , defaultPage : String
    , portals : List Portal
    }


dmyToYmd : String -> String
dmyToYmd dmy =
    let
        parts : List String
        parts =
            String.split "/" dmy
    in
    case parts of
        [ day, month, year ] ->
            year ++ "-" ++ month ++ "-" ++ day

        _ ->
            dmy


dateFromRCString : String -> Maybe Date
dateFromRCString str =
    str
        |> dmyToYmd
        |> Date.fromIsoString
        |> Result.toMaybe


type alias Research r =
    { r
        | id : ExpositionID
        , title : String
        , keywords : List KeywordString
        , created : String
        , author : Author
        , issueId : Maybe Int
        , publicationStatus : PublicationStatus -- should be string?
        , publication : Maybe Date
        , thumbnail : Maybe String
        , abstract : Maybe String
        , defaultPage : String
        , portals : List Portal
    }


calcStatus : Research r -> PublicationStatus
calcStatus research =
    case research.publicationStatus of
        InProgress ->
            InProgress

        _ ->
            Published


type alias ReverseKeywordDict a =
    Dict String (List { a | keywords : List KeywordString })


reverseKeywordDict : List { a | keywords : List KeywordString } -> Dict String (List { a | keywords : List KeywordString })
reverseKeywordDict research =
    -- note this is case insensitive now!
    let
        addExpToKeyword xpo kw currentDict =
            Dict.update (kw |> String.toLower)
                (\value ->
                    case value of
                        Nothing ->
                            Just [ xpo ]

                        Just lst ->
                            Just (xpo :: lst)
                )
                currentDict

        addResearchToDict exp currentDict =
            -- this exposition has keywords k1 k2 k3
            List.foldl (addExpToKeyword exp) currentDict (exp.keywords |> List.map KeywordString.toString)
    in
    List.foldl addResearchToDict Dict.empty research


findResearchWithKeywords kw dict research =
    let
        findKw k =
            k |> String.toLower |> (\s -> Dict.get s dict) |> Maybe.withDefault []

        getId exp =
            exp.id

        {- for each keyword, return the id's that have it, now take the union of those sets of ids -}
        {- use the ids to fetch the expositions -}
        intersectionOfNonempty : Set comparable -> List (Set comparable) -> List comparable
        intersectionOfNonempty first rest =
            List.foldl Set.union first rest |> Set.toList

        combineResults : List (Set comparable) -> List comparable
        combineResults results =
            case results of
                [] ->
                    []

                x :: xs ->
                    intersectionOfNonempty x xs
    in
    case kw |> Set.toList of
        [] ->
            research

        kws ->
            let
                ids =
                    kws
                        |> List.map (\r -> r |> findKw |> List.map getId |> Set.fromList)
                        |> combineResults

                -- the full set is the mempty of combinatory filters
            in
            research |> List.filter (\exp -> List.member exp.id ids)


findResearchWithTitle : String -> List (Research r) -> List (Research r)
findResearchWithTitle q lst =
    let
        f : Research r -> Bool
        f r =
            r.title |> String.toLower |> String.contains (String.toLower q)
    in
    case q of
        "" ->
            lst

        _ ->
            List.filter f lst


findResearchWithAuthor : String -> List (Research r) -> List (Research r)
findResearchWithAuthor qauthor lst =
    let
        f : Research r -> Bool
        f r =
            r.author |> getName |> String.toLower |> String.contains (String.toLower qauthor)
    in
    case qauthor of
        "" ->
            lst

        _ ->
            List.filter f lst


rcDateToPosix : String -> Result String Time.Posix
rcDateToPosix rcdate =
    --
    case rcdate |> String.split "/" of
        [ d, m, y ] ->
            [ d, m, y ] |> String.join "-" |> Iso8601.toTime |> Result.mapError (always "nope")

        _ ->
            Err "couldn't parse this"


rcDateToRataDie : String -> Result String Date
rcDateToRataDie rcdate =
    case rcdate |> String.split "/" of
        [ y, m, d ] ->
            [ y, m, d ] |> String.join "-" |> Date.fromIsoString

        _ ->
            Err <| "expected ISO-8601 date, but got instead: " ++ rcdate


type Compare
    = Smaller



-- comparePosix : Posix -> Posix -> Compare
-- comparePosix p1 p2 =
--     let
--         ( m1, m2 ) =
--             ( Time.posixToMillis p1, Time.posixToMillis p2 )
--     in
--     if m1 == m2 then
--         Equal
--     else if m1 > m2 then
--         Bigger
--     else
--         Smaller


findResearchAfter date lst =
    let
        test research =
            case research.publication of
                Just researchdate ->
                    List.member (Date.compare researchdate date) [ GT, EQ ]

                Nothing ->
                    -- if there is no date it is not included in results
                    False
    in
    List.filter test lst


findResearchBefore date lst =
    let
        test : Research r -> Bool
        test research =
            case research.publication of
                Just researchdate ->
                    Date.compare researchdate date == LT

                Nothing ->
                    False
    in
    List.filter test lst


findResearchWithPortal : String -> List (Research r) -> List (Research r)
findResearchWithPortal portalq lst =
    -- let
    --     _ =
    --         Debug.log portalq "portalq"
    -- in
    case portalq of
        "" ->
            lst

        nonemptyq ->
            let
                f : Research r -> Bool
                f research =
                    case research.portals of
                        [] ->
                            False

                        somePortals ->
                            let
                                names =
                                    somePortals |> List.map (.name >> String.toLower)
                            in
                            names |> List.any (\p -> p == (nonemptyq |> String.toLower))
            in
            List.filter f lst



-- maybe the not found error could also by a type?


findExpositionWithId : ExpositionID -> List (Research r) -> Result String (Research r)
findExpositionWithId id lst =
    case lst |> List.filter (\r -> r.id == id) |> List.head of
        Just exp ->
            Ok exp

        Nothing ->
            Err (String.fromInt id ++ " not found")


containsIgnoreCase : String -> String -> Bool
containsIgnoreCase needle haystack =
    String.contains (String.toLower needle) (String.toLower haystack)


findResearchWithAbstract : String -> List (Research r) -> List (Research r)
findResearchWithAbstract abstractQ lst =
    lst
        |> List.filter
            (\r ->
                case r.abstract of
                    Nothing ->
                        False

                    Just "" ->
                        False

                    Just nonEmptyString ->
                        containsIgnoreCase abstractQ nonEmptyString
            )
