module EnrichedResearch exposing
    ( ResearchWithKeywords
    , decodeExpositionResult
    , decoder
    , encodeExpositionResult
    , encodeResearchWithKeywords
    , enrich
    , keywordSet
    , renderAbstract
    )

import Array exposing (Array)
import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, text)
import Element.Font as Font
import Json.Decode exposing (Decoder, field, int, maybe, string)
import Json.Decode.Extra as JDE
import Json.Encode
import KeywordString exposing (KeywordString)
import Regex exposing (Regex)
import Research exposing (Author, ExpositionID, Portal, PublicationStatus, Research)
import RichAbstract exposing (..)
import Screenshots
import Toc


type alias ResearchWithKeywords =
    { id : ExpositionID
    , title : String
    , keywords : List KeywordString
    , created : String
    , createdDate : Date
    , author : Author
    , issueId : Maybe Int
    , publicationStatus : PublicationStatus -- should be string?
    , publication : Maybe Date
    , connectedTo : List Portal
    , thumbnail : Maybe String
    , abstract : Maybe String
    , defaultPage : String
    , portals : List Portal
    , abstractWithKeywords : AbstractWithKeywords
    , toc : Maybe Toc.ExpositionToc
    , screenshots : Maybe Screenshots.Exposition
    }


type Rank
    = Unranked
    | Rank Int


mkResearchWithKeywords :
    ExpositionID
    -> String
    -> List KeywordString
    -> String
    -> Date
    -> Author
    -> Maybe Int
    -> PublicationStatus
    -> Maybe Date
    -> Maybe String
    -> Maybe String
    -> String
    -> List Portal
    -> List Portal
    -> AbstractWithKeywords
    -> Maybe Toc.ExpositionToc
    -> Maybe Screenshots.Exposition
    -> ResearchWithKeywords
mkResearchWithKeywords id title keywords created createdDate authr issueId publicationStatus publication thumbnail abstract defaultPage portals connectedToPortals abstractWithKw simpleToc screenshots =
    { id = id
    , title = title
    , keywords = keywords
    , created = created
    , createdDate = createdDate
    , author = authr
    , issueId = issueId
    , publicationStatus = publicationStatus -- should be string?
    , publication = publication
    , thumbnail = thumbnail
    , abstract = abstract
    , defaultPage = defaultPage
    , portals = portals
    , connectedTo = connectedToPortals
    , abstractWithKeywords = abstractWithKw
    , toc = simpleToc
    , screenshots = screenshots
    }


keywordSet : List (Research r) -> Research.KeywordSet
keywordSet researchlist =
    List.foldr
        (\research set ->
            List.foldr Research.insert set research.keywords
        )
        Research.emptyKeywordSet
        researchlist


lookupToc : Dict ExpositionID Toc.ExpositionToc -> Research r -> Maybe Toc.ExpositionToc
lookupToc toc_dict research =
    Dict.get research.id toc_dict


enrich : Dict ExpositionID Toc.ExpositionToc -> List (Research r) -> Research.KeywordSet -> Screenshots.RCScreenshots -> List ResearchWithKeywords
enrich toc lst kwSet screenshotsData =
    let
        kwList =
            kwSet |> Research.toList |> List.map (Research.kwName >> KeywordString.fromString)

        abstractWithKeywords e =
            e
                |> fancyAbstract kwList

        getToc e =
            lookupToc toc e

        getscreens e =
            Screenshots.getScreenshots e.id screenshotsData

        toResearchWithKw exp =
            researchWithTocAndKeywords (getToc exp) exp (abstractWithKeywords exp) (getscreens exp)
    in
    lst |> List.map toResearchWithKw


researchWithTocAndKeywords : Maybe Toc.ExpositionToc -> Research r -> AbstractWithKeywords -> Maybe Screenshots.Exposition -> ResearchWithKeywords
researchWithTocAndKeywords toc expo kwAbstract screenshots =
    { id = expo.id
    , title = expo.title
    , keywords = expo.keywords
    , created = expo.created
    , createdDate = expo.created |> Date.fromIsoString |> Result.withDefault (Date.fromRataDie 0)
    , author = expo.author
    , issueId = expo.issueId
    , publicationStatus = expo.publicationStatus
    , publication = expo.publication
    , thumbnail = expo.thumbnail
    , abstract = expo.abstract
    , defaultPage = expo.defaultPage
    , portals = expo.portals
    , connectedTo = expo.portals
    , abstractWithKeywords = kwAbstract
    , toc = toc
    , screenshots = screenshots
    }


encodeResearchWithKeywords : ResearchWithKeywords -> Json.Encode.Value
encodeResearchWithKeywords exp =
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
                        ( "published", int (Date.toRataDie p) )
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

        toc =
            exp.toc
                |> Maybe.map
                    (\t ->
                        ( "toc", Toc.encodeToc t )
                    )

        screenshots =
            exp.screenshots
                |> Maybe.map
                    (\s ->
                        ( "screenshots", Screenshots.encodeExposition s )
                    )

        createdDate =
            exp.createdDate |> Date.toRataDie |> Json.Encode.int
    in
    Json.Encode.object
        ([ ( "type", string "exposition" )
         , ( "id", int exp.id )
         , ( "created", string exp.created )
         , ( "createdDate", createdDate )
         , ( "title", string exp.title )
         , ( "keywords", list string (List.map KeywordString.toString exp.keywords) )
         , ( "author", Research.encodeAuthor exp.author )
         , ( "status", Research.publicationstatus exp.publicationStatus )
         , ( "defaultPage", string exp.defaultPage )
         , ( "portals", list Research.encodePortal exp.portals )
         , ( "connectedTo", list Research.encodePortal exp.portals )
         , ( "abstractWithKeywords", encodeAbstract exp.abstractWithKeywords )
         ]
            |> maybeAppend issueId
            |> maybeAppend publication
            |> maybeAppend thumbnail
            |> maybeAppend abstract
            |> maybeAppend toc
            |> maybeAppend screenshots
        )



-- AS IN INTERNAL_RESEARCH.JSON


decoder : Decoder ResearchWithKeywords
decoder =
    let
        researchPublicationStatus : ResearchWithKeywords -> ResearchWithKeywords
        researchPublicationStatus research =
            { research | publicationStatus = Research.calcStatus research }

        statusFromString : String -> PublicationStatus
        statusFromString statusString =
            case statusString of
                "published" ->
                    Research.Published

                "progress" ->
                    Research.InProgress

                _ ->
                    Research.Undecided
    in
    Json.Decode.map researchPublicationStatus <|
        (Json.Decode.succeed
            mkResearchWithKeywords
            |> JDE.andMap (field "id" int)
            |> JDE.andMap (field "title" string)
            |> JDE.andMap (field "keywords" (Json.Decode.list string) |> Json.Decode.map (List.map KeywordString.fromString))
            |> JDE.andMap (field "created" string)
            |> JDE.andMap (field "createdDate" (Json.Decode.int |> Json.Decode.map Date.fromRataDie))
            |> JDE.andMap (field "author" Research.author)
            |> JDE.andMap (maybe (field "issue" <| field "id" int))
            |> JDE.andMap (Json.Decode.map statusFromString (field "status" string))
            |> JDE.andMap (maybe (field "published" (int |> Json.Decode.map Date.fromRataDie)))
            |> JDE.andMap (maybe (field "thumbnail" string))
            |> JDE.andMap (maybe (field "abstract" string))
            |> JDE.andMap (field "defaultPage" string)
            |> JDE.andMap (field "portals" (Json.Decode.list Research.rcPortalDecoder))
            |> JDE.andMap (field "connectedTo" (Json.Decode.list Research.rcPortalDecoder))
            |> JDE.andMap (field "abstractWithKeywords" decodeAbstractWithKeywords)
            |> JDE.andMap (maybe (field "toc" Toc.decodeToc))
            |> JDE.andMap (maybe (field "screenshots" Screenshots.decodeExposition))
        )



-- Abstract with parsed keywords


{-|

    This checks if a keyword is in the abstract.

-}
isKwInAbstract : String -> KeywordString -> Bool
isKwInAbstract abstract kws =
    let
        -- to make sure keywords are also found if it has a !.? etc.. at the end
        kw : String
        kw =
            " " ++ KeywordString.toString kws ++ "[!.,? ;:]"

        maybeRegex : Maybe Regex.Regex
        maybeRegex =
            Regex.fromString kw

        regex : Regex.Regex
        regex =
            Maybe.withDefault Regex.never maybeRegex
    in
    Regex.contains regex abstract


sliceAbstract : Maybe String -> Int -> String
sliceAbstract abs max =
    let
        isGreaterThan : Int -> Int -> Bool
        isGreaterThan mx value =
            value > mx

        abstract =
            Maybe.withDefault "" abs

        fullStopsInAbstract =
            String.indexes "." abstract

        fullStopsAfterMax =
            List.filter (isGreaterThan max) fullStopsInAbstract

        firstFullStopAfterMax =
            Maybe.withDefault max (List.head fullStopsAfterMax)
    in
    String.left (firstFullStopAfterMax + 1) abstract


findKwsInAbstract : List KeywordString -> String -> ( List Int, List String )
findKwsInAbstract kws shortAbstract =
    let
        kwsInAbstract =
            List.map (findKwInAbstract shortAbstract) kws

        kwsSorted =
            List.drop 1 (List.sort kwsInAbstract)
    in
    List.unzip kwsSorted


{-|

    This returns, given an abstract and keyword, the index and the keyword that was matched as a string.

-}
findKwInAbstract : String -> KeywordString -> ( Int, String )
findKwInAbstract abstract kw =
    let
        extractIndex : Maybe Regex.Match -> Int
        extractIndex match =
            case match of
                Nothing ->
                    0

                Just m ->
                    m.index

        keyword : String
        keyword =
            KeywordString.toString kw

        key : String
        key =
            " " ++ keyword ++ "[!.,? ;:]"

        maybeRegex : Maybe Regex
        maybeRegex =
            Regex.fromString key

        regex : Regex
        regex =
            Maybe.withDefault Regex.never maybeRegex

        finds : List Regex.Match
        finds =
            Regex.find regex abstract

        first : Maybe Regex.Match
        first =
            List.head finds

        kwStart : Int
        kwStart =
            extractIndex first
    in
    ( kwStart, keyword )


{-|

    Given a list of keywords, tries to find if it is a subkeyword

-}
isSubkeyword : List String -> Int -> Bool
isSubkeyword keywords index =
    let
        kws =
            Array.fromList keywords

        kw =
            Maybe.withDefault "" (Array.get index kws)

        first =
            Array.slice 0 index kws

        second =
            Array.slice (index + 1) (Array.length kws) kws

        arr =
            Array.append first second

        bools =
            Array.map (String.contains kw) arr

        list =
            Array.toList bools
    in
    List.member True list


fancyAbstract : List KeywordString -> Research r -> AbstractWithKeywords
fancyAbstract allKeywords research =
    let
        abstractMax =
            300

        shortAbstract =
            sliceAbstract research.abstract abstractMax

        kws =
            List.filter (isKwInAbstract shortAbstract) allKeywords

        foundKws =
            findKwsInAbstract kws shortAbstract

        abstractIndexes =
            Tuple.first foundKws

        abstractKeywords =
            Tuple.second foundKws

        series =
            List.range 0 (List.length abstractKeywords)

        subKeywords =
            List.map (isSubkeyword abstractKeywords) series

        kwina =
            List.map (parsedAbstract abstractIndexes subKeywords abstractKeywords shortAbstract) series

        abstract =
            List.concat kwina

        keywordLessBugFix =
            case RichAbstract.asString abstract of
                "{{}}" ->
                    -- DANGER:
                    -- I think there is some problem if there is no keywords, the rest of the abstract goes missing, and a {{}} is returned when encoding it.
                    -- Instead of fixing the bug, we just take the abstract. This looks risky to me, but don't have time to resolve it in a better way
                    [ AbsText shortAbstract ]

                _ ->
                    abstract
    in
    keywordLessBugFix


parsedAbstract : List Int -> List Bool -> List String -> String -> Int -> AbstractWithKeywords
parsedAbstract indexes subkeywords keywords abstract which =
    let
        kwsLength =
            List.length keywords

        idx =
            Array.fromList indexes

        kws =
            Array.fromList keywords

        subs =
            Array.fromList subkeywords

        isSub =
            Maybe.withDefault False (Array.get which subs)

        firstk =
            Maybe.withDefault "-1" (Array.get 0 kws)
    in
    if which == 0 then
        -- first kw
        let
            k =
                Maybe.withDefault -1 (Array.get which idx)

            -- I think this happens when the abstact is a white space
            -- this matches somehow the "?" keyword, which is then dropped creating an empty list
            keyw =
                Maybe.withDefault "" (Array.get which kws)

            kwlength =
                String.length keyw

            prevk =
                Maybe.withDefault 0 (Array.get (which - 1) idx)

            prevkeyw =
                Maybe.withDefault "" (Array.get (which - 1) kws)

            prevkwlength =
                String.length prevkeyw

            sliceLeft =
                String.slice (prevk + prevkwlength) (k + 1) abstract
        in
        if isSub == True then
            [ AbsText sliceLeft ]

        else
            [ AbsText sliceLeft, AbsKw keyw ]

    else if which == kwsLength then
        -- append abstract end
        let
            k =
                Maybe.withDefault -1 (Array.get (which - 1) idx)

            keyw =
                Maybe.withDefault "-1" (Array.get (which - 1) kws)

            kwlength =
                String.length keyw

            sliceRight =
                String.dropLeft (k + kwlength + 1) abstract
        in
        [ AbsText sliceRight ]

    else
        -- slice abstract snippet + insert kw link
        let
            k =
                Maybe.withDefault -1 (Array.get which idx)

            keyw =
                Maybe.withDefault ">>>>>>>>" (Array.get which kws)

            kwlength =
                String.length keyw

            prevk =
                Maybe.withDefault 0 (Array.get (which - 1) idx)

            prevkeyw =
                Maybe.withDefault "" (Array.get (which - 1) kws)

            prevkwlength =
                String.length prevkeyw

            sliceLeft =
                String.slice (prevk + prevkwlength + 1) (k + 1) abstract

            strToKw =
                stringToKeyword keyw
        in
        if isSub == True then
            [ AbsText sliceLeft ]

        else
            [ AbsText sliceLeft, AbsKw keyw ]


gray : Element.Color
gray =
    Element.rgb 0.5 0.5 0.5


abstractStyle : List (Element.Attr () msg)
abstractStyle =
    [ Font.size 12 ]


stringToKeyword : String -> Element msg
stringToKeyword str =
    Element.link (abstractStyle ++ [ Font.underline, Font.color gray ]) <|
        { label = Element.text str
        , url = "/#/research/search/list?author&keyword=" ++ str ++ " "
        }


renderAbstract : AbstractWithKeywords -> Element msg
renderAbstract abstract =
    Element.paragraph (Element.padding 0 :: Element.width Element.fill :: abstractStyle)
        (abstract
            |> List.map
                (\elem ->
                    case elem of
                        AbsKw kw ->
                            stringToKeyword kw

                        AbsText txt ->
                            text txt
                )
        )



-- TODO move part of this to screenshots.elm


decodeExpositionResult : Json.Decode.Decoder (Result String ResearchWithKeywords)
decodeExpositionResult =
    let
        decodeOk =
            field "Ok" decoder |> Json.Decode.map Ok

        decodeErr =
            field "Err" string |> Json.Decode.map Err
    in
    Json.Decode.oneOf [ decodeOk, decodeErr ]


encodeExpositionResult : Result String ResearchWithKeywords -> Json.Encode.Value
encodeExpositionResult exp =
    case exp of
        Ok e ->
            Json.Encode.object [ ( "Ok", encodeResearchWithKeywords e ) ]

        Err error ->
            Json.Encode.object [ ( "Err", Json.Encode.string error ) ]



-- makeSnippet : List Int -> List Bool -> List String -> String -> Int -> List (Element msg)
-- makeSnippet indexes subkeywords keywords abstract which =
--     let
--         kwsLength =
--             List.length keywords
--         idx =
--             Array.fromList indexes
--         kws =
--             Array.fromList keywords
--         subs =
--             Array.fromList subkeywords
--         isSub =
--             Maybe.withDefault False (Array.get which subs)
--         firstk =
--             Maybe.withDefault "-1" (Array.get 0 kws)
--     in
--     if which == 0 then
--         -- first kw
--         let
--             k =
--                 Maybe.withDefault -1 (Array.get which idx)
--             -- I think this happens when the abstact is a white space
--             -- this matches somehow the "?" keyword, which is then dropped creating an empty list
--             keyw =
--                 Maybe.withDefault "!!! This is an empty list !!!" (Array.get which kws)
--             kwlength =
--                 String.length keyw
--             prevk =
--                 Maybe.withDefault 0 (Array.get (which - 1) idx)
--             prevkeyw =
--                 Maybe.withDefault "" (Array.get (which - 1) kws)
--             prevkwlength =
--                 String.length prevkeyw
--             sliceLeft =
--                 String.slice (prevk + prevkwlength) (k + 1) abstract
--             strToKw =
--                 stringToKeyword keyw
--         in
--         if isSub == True then
--             [ text sliceLeft ]
--         else
--             [ text sliceLeft, strToKw ]
--     else if which == kwsLength then
--         -- append abstract end
--         let
--             k =
--                 Maybe.withDefault -1 (Array.get (which - 1) idx)
--             keyw =
--                 Maybe.withDefault "-1" (Array.get (which - 1) kws)
--             kwlength =
--                 String.length keyw
--             sliceRight =
--                 String.dropLeft (k + kwlength + 1) abstract
--         in
--         [ text sliceRight ]
--     else
--         -- slice abstract snippet + insert kw link
--         let
--             k =
--                 Maybe.withDefault -1 (Array.get which idx)
--             keyw =
--                 Maybe.withDefault ">>>>>>>>" (Array.get which kws)
--             kwlength =
--                 String.length keyw
--             prevk =
--                 Maybe.withDefault 0 (Array.get (which - 1) idx)
--             prevkeyw =
--                 Maybe.withDefault "" (Array.get (which - 1) kws)
--             prevkwlength =
--                 String.length prevkeyw
--             sliceLeft =
--                 String.slice (prevk + prevkwlength + 1) (k + 1) abstract
--             strToKw =
--                 stringToKeyword keyw
--         in
--         if isSub == True then
--             [ text sliceLeft ]
--         else
--             [ text sliceLeft, strToKw ]
