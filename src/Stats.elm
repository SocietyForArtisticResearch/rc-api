module Stats exposing (..)

import Dict exposing (Dict)
import Element
import Element.Background
import Element.Font as Font
import Expo exposing (Media(..), PageType(..), pageTypeOfString, stringOfPageType)
import Http
import Json.Decode as D exposing (field, int, maybe)
import Json.Encode as E
import List.Extra
import RCStyles
import Research as RC exposing (Research)
import Tools exposing (ToolType(..))
import Url.Builder exposing (absolute)
import Utils exposing (defaultPadding)


baseurl =
    "https://map.rcdata.org"


type StatsSort
    = MostTools ToolType
    | Overlap
    | MostPages
    | OveralTools
    | WhiteSpace
    | Spacing
    | Uniformity
    | Alignment
    | Horizontal
    | MostLinks LinkType


type LinkType
    = NavLink
    | OtherExpoLink
    | ReferenceLink
    | ExternalLink
    | Broken



-- TODO, this is because inconsistent naming in API not using official tool names


rcpatcher : String -> String
rcpatcher str =
    case str of
        "tool-simple-text" ->
            "tool-simpletext"

        _ ->
            str


statsAsOptions : List ( String, Maybe StatsSort )
statsAsOptions =
    [ ( "", Nothing )
    , ( "most tools", Just OveralTools )
    , ( "video tools", Just (MostTools Tools.VideoTool) )
    , ( "audio tools", Just (MostTools Tools.AudioTool) )
    , ( "text tools", Just (MostTools Tools.SimpleTextTool) )
    , ( "html tools", Just (MostTools Tools.HtmlTool) )
    , ( "pdf tools", Just (MostTools Tools.PdfTool) )
    , ( "shape tools", Just (MostTools Tools.ShapeTool) )
    , ( "slideshow tools", Just (MostTools Tools.SlideshowTool) )
    , ( "image tools", Just (MostTools Tools.PictureTool) )
    , ( "tool overlap", Just Overlap )
    , ( "most pages", Just MostPages )
    , ( "amount of white space", Just WhiteSpace )
    , ( "spacing score", Just Spacing )
    , ( "uniformity of tool size", Just Uniformity )
    , ( "alignment of tool borders", Just Alignment )
    , ( "horizontal vs vertical", Just Horizontal )
    , ( "links external", Just (MostLinks ExternalLink) )
    , ( "links internal", Just (MostLinks NavLink) )

    --, ( "links broken", Just (MostLinks Broken) )
    , ( "links reference", Just (MostLinks ReferenceLink) )
    , ( "links other expositions", Just (MostLinks OtherExpoLink) )
    ]


viewStatsSort : Maybe StatsSort -> String
viewStatsSort ss =
    case ss of
        Nothing ->
            ""

        Just (MostTools Tools.VideoTool) ->
            "number of video tools"

        Just (MostTools Tools.AudioTool) ->
            "number of audio tools"

        Just (MostTools Tools.SimpleTextTool) ->
            "number of text tools"

        Just (MostTools Tools.HtmlTool) ->
            "number of html tools"

        Just (MostTools Tools.PdfTool) ->
            "number of pdf tools"

        Just (MostTools Tools.ShapeTool) ->
            "number of shape tools"

        Just (MostTools Tools.SlideshowTool) ->
            "number of slideshow tools"

        Just (MostTools Tools.PictureTool) ->
            "number of image tools"

        Just (MostTools Tools.NoteTool) ->
            "number of note tools"

        Just (MostTools Tools.EmbedTool) ->
            "number of embed tools"

        Just Overlap ->
            "overlap of tools"

        Just MostPages ->
            "total number of pages"

        Just OveralTools ->
            "total number of tools"

        Just WhiteSpace ->
            "amount of whitespace"

        Just Spacing ->
            "spacing between tools"

        Just Uniformity ->
            "uniformity of tool size"

        Just Alignment ->
            "how aligned tools are"

        Just Horizontal ->
            "how horizontal or vertical is the exposition"

        Just (MostLinks ExternalLink) ->
            "links external"

        Just (MostLinks NavLink) ->
            "links internal/same exposition"

        Just (MostLinks OtherExpoLink) ->
            "links other exposition"

        Just (MostLinks ReferenceLink) ->
            "links reference"

        Just (MostLinks Broken) ->
            "links broken"


encodeStatsSort : StatsSort -> E.Value
encodeStatsSort ss =
    let
        constant str =
            E.object [ ( str, E.null ) ]
    in
    case ss of
        MostTools tt ->
            E.object
                [ ( "toolCount", E.string (tt_string_short tt) ) ]

        Overlap ->
            constant "overlap"

        MostPages ->
            constant "mostpages"

        OveralTools ->
            constant "overaltools"

        WhiteSpace ->
            constant "whitespace"

        Spacing ->
            constant "spacing"

        Alignment ->
            constant "alignment"

        Uniformity ->
            constant "uniformity"

        Horizontal ->
            constant "horizontal"

        MostLinks linktype ->
            E.object [ ( "linkCount", E.string (link_type_to_string linktype) ) ]


decodeLinkType : String -> Maybe LinkType
decodeLinkType str =
    case str of
        "extlink" ->
            Just ExternalLink

        "navlink" ->
            Just NavLink

        "reflink" ->
            Just ReferenceLink

        "brokenlink" ->
            Just Broken

        "otherexpolink" ->
            Just OtherExpoLink

        _ ->
            Nothing


decodeStatsSort : D.Decoder StatsSort
decodeStatsSort =
    D.oneOf
        [ field "toolCount" D.string |> D.map (tt_of_string >> Result.toMaybe >> Maybe.map MostTools >> Maybe.withDefault (MostTools VideoTool))
        , field "overlap" (D.succeed Overlap)
        , field "mostpages" (D.succeed MostPages)
        , field "overaltools" (D.succeed OveralTools)
        , field "whitespace" (D.succeed WhiteSpace)
        , field "spacing" (D.succeed Spacing)
        , field "alignment" (D.succeed Alignment)
        , field "uniformity" (D.succeed Uniformity)
        , field "horizontal" (D.succeed Horizontal)
        , field "linkCount" D.string |> D.map (link_type_from_string >> Result.toMaybe >> Maybe.withDefault (MostLinks OtherExpoLink))
        ]


type StatExpo a
    = StatExpo Stats (RC.Research a)



-- sortResults : List (Research a) -> List Stats -> List StatExpo
-- sortResults =


sortByTool : ToolType -> String
sortByTool tt =
    Url.Builder.crossOrigin baseurl [ "api", "sort-by-tool" ] [ Url.Builder.string "tool" (tt |> Tools.toRClass), Url.Builder.int "n" 10000 ]


sortBy : StatsSort -> String
sortBy ss =
    let
        by_metric : String -> String
        by_metric metric =
            Url.Builder.crossOrigin baseurl [ "api", "sort-by-metric" ] [ Url.Builder.string "metric" metric, Url.Builder.int "n" 10000 ]

        by_link_type : String -> String
        by_link_type lt_str =
            Url.Builder.crossOrigin baseurl [ "api", "sort-by-link" ] [ Url.Builder.string "link_type" lt_str, Url.Builder.int "n" 10000 ]
    in
    case ss of
        Overlap ->
            Url.Builder.crossOrigin baseurl [ "api", "sort-by-metric" ] [ Url.Builder.string "metric" "overlap_percentage", Url.Builder.int "n" 10000 ]

        MostTools tt ->
            Url.Builder.crossOrigin baseurl [ "api", "sort-by-tool" ] [ Url.Builder.string "tool" (tt |> Tools.toRClass |> rcpatcher), Url.Builder.int "n" 10000 ]

        MostPages ->
            Url.Builder.crossOrigin baseurl [ "api", "highest-total-pages" ] [ Url.Builder.int "n" 10000 ]

        OveralTools ->
            Url.Builder.crossOrigin baseurl [ "api", "highest-total-tools" ] [ Url.Builder.int "n" 10000 ]

        WhiteSpace ->
            by_metric "white_space_percentage"

        Spacing ->
            by_metric "spacing-score"

        Alignment ->
            by_metric "alignment_score"

        Uniformity ->
            by_metric "size_uniformity_score"

        Horizontal ->
            by_metric "horizontal_vertical_ratio"

        MostLinks ExternalLink ->
            by_link_type "external"

        MostLinks NavLink ->
            by_link_type "same_exposition"

        MostLinks Broken ->
            by_link_type "broken"

        MostLinks OtherExpoLink ->
            by_link_type "other_expositions"

        MostLinks ReferenceLink ->
            by_link_type "references"



--Url.Builder.crossOrigin baseurl [ "api", "horizontal_vertical_ratio" ] [ Url.Builder.int "n" 10000 ]
--- [ baseurl, "sort-by-tool", Tools.toRClass tt ] |> String.join "/"
-- getMostVideoTools : (Result Http.Error (List Stats) -> msg) -> Cmd msg
-- getMostVideoTools toMessage =
--     let
--         props =
--             { url = sortByTool Tools.VideoTool
--             , expect = Http.expectJson toMessage (D.list decodeStats)
--             }
--     in
--     Http.get props


getStatsSort : StatsSort -> (Result Http.Error (List Stats) -> msg) -> Cmd msg
getStatsSort statsSort toMessage =
    let
        props =
            { url = sortBy statsSort
            , expect = Http.expectJson toMessage (D.list decodeStats)
            }
    in
    Http.get props


getExpoStats : RC.ExpositionID -> (Result Http.Error Stats -> msg) -> Cmd msg
getExpoStats expoId toMessage =
    let
        props =
            { url = Url.Builder.crossOrigin baseurl [ "api", "exposition", String.fromInt expoId ] []
            , expect = Http.expectJson toMessage decodeStats
            }
    in
    Http.get props


type alias Stats =
    { id : Int
    , toolStats : ToolCounts
    , metrics : Maybe Metrics
    , format : Format
    , numberOfPages : Int
    , defaultPage : String
    , totalNumberOfTools : Int
    , linkCounts : Maybe LinkCounts
    }


type alias LinkCounts =
    { broken : Int
    , external : Int
    , other_expositions : Int
    , same_exposition : Int
    , references : Int
    }



-- specific tools


type alias ToolCounts =
    { video : Int
    , audio : Int
    , text : Int
    , html : Int
    , pdf : Int
    , shape : Int
    , slideshow : Int
    , image : Int
    }



-- To be extended later


type alias Metrics =
    { alignmentScore : Float
    , spacingScore : Float
    , sizeUniformityScore : Float
    , overlapPercentage : Float -- INTERESTING
    , whiteSpacePercentage : Float -- interesting
    , horizontalVerticalRatio : Float --
    , overallRegularScore : Float
    }



-- which editor was used


type alias Format =
    { defaultPageFormat : Maybe PageType
    , pageFormats : Dict.Dict Int PageType
    }


type DetailLevel
    = MinDetail
    | MaxDetail


displayFormat : DetailLevel -> Format -> String
displayFormat detail format =
    case detail of
        MaxDetail ->
            format.defaultPageFormat |> Maybe.map Expo.displayPageType |> Maybe.withDefault ""

        MinDetail ->
            format.pageFormats
                |> Dict.toList
                |> List.map (Tuple.second >> Expo.displayPageType)
                |> List.Extra.unique
                |> String.join ","


mkFormatOnlyDefault : Maybe PageType -> Format
mkFormatOnlyDefault pt =
    { defaultPageFormat = pt
    , pageFormats = Dict.empty
    }


decodeStats : D.Decoder Stats
decodeStats =
    let
        formatDecoder =
            -- TODO: think this can be done simpler, with a D.maybe
            D.oneOf
                [ field "default-page-type" (decodePageType |> D.map (Just >> mkFormatOnlyDefault))
                , D.succeed (mkFormatOnlyDefault Nothing)
                ]

        maybeLinkCounts =
            D.oneOf
                [ D.field "link-counts" decodeLinkCounts |> D.map Just
                , D.succeed Nothing
                ]
    in
    D.map8 Stats
        (field "id" (D.string |> D.map (String.toInt >> Maybe.withDefault -1)))
        (field "tool-counts" decodeToolCounts)
        (maybe (field "metrics" decodeMetrics))
        formatDecoder
        (field "number-of-pages" int)
        (field "default-page" D.string)
        (field "total-number-of-tools" int)
        maybeLinkCounts


encodeStats : Stats -> E.Value
encodeStats stats =
    let
        mformat =
            stats.format.defaultPageFormat |> Maybe.map (\dpf -> ( "default-page-format", E.string (stringOfPageType dpf) ))

        mMetrics =
            stats.metrics |> Maybe.map (\metrics -> ( "metrics", encodeMetrics metrics ))

        mLinkCounts =
            stats.linkCounts |> Maybe.map (\lc -> ( "link-counts", encodeLinkCounts lc ))
    in
    E.object
        ([ ( "id", E.string (String.fromInt stats.id) )
         , ( "tool-counts", encodeToolStats stats.toolStats )
         , ( "number-of-pages", E.int stats.numberOfPages )
         , ( "default-page", E.string stats.defaultPage )
         , ( "total-number-of-tools", E.int stats.totalNumberOfTools )
         ]
            |> prependMaybe mMetrics
            |> prependMaybe mformat
            |> prependMaybe mLinkCounts
        )



-- decodeStats : D.Decoder Stats
-- decodeStats =
--     D.map3 Stats
--         (field "toolcounts" decodeToolCounts)
--         (field "metrics" decodeMetrics)
--         (field "format" decodeFormat)


decodePageType : D.Decoder PageType
decodePageType =
    D.string
        |> D.andThen
            (\str ->
                case str |> Expo.pageTypeOfString of
                    Nothing ->
                        D.fail ("unknown page type" ++ str)

                    Just pt ->
                        D.succeed pt
            )


optionalIntField : String -> D.Decoder Int
optionalIntField name =
    field name int |> maybe |> D.map (Maybe.withDefault 0)


maybeZeroCount : String -> D.Decoder Int
maybeZeroCount name =
    D.oneOf
        [ D.field name int
        , D.succeed 0
        ]


decodeToolCounts : D.Decoder ToolCounts
decodeToolCounts =
    D.map8 ToolCounts
        (maybeZeroCount "tool-video")
        (maybeZeroCount "tool-audio")
        (maybeZeroCount "tool-text")
        (maybeZeroCount "tool-html")
        (maybeZeroCount "tool-pdf")
        (maybeZeroCount "tool-shape")
        (maybeZeroCount "tool-slideshow")
        (maybeZeroCount "tool-picture")



-- decodeToolCountsApi : D.Decoder ToolCounts
-- decodeToolCountsApi =
--     D.map8 ToolCounts
--         (D.field "tool-video" int)
--         (D.field "tool-audio" int)
--         (D.field "tool-simple-text" int)
--         (D.field "tool-text" int)
--         (D.field "tool-pdf" int)
--         (D.field "tool-shape" int)
--         (D.field "tool-slideshow" int)
--         (D.field "tool-picture" int)


decodeMetrics : D.Decoder Metrics
decodeMetrics =
    let
        float =
            D.float
    in
    D.map7 Metrics
        (field "alignment_score" float)
        (field "spacing_score" float)
        (field "size_uniformity_score" float)
        (field "overlap_percentage" float)
        (field "white_space_percentage" float)
        (field "horizontal_vertical_ratio" float)
        (field "overall_regular_score" float)


encodeToolStats : ToolCounts -> E.Value
encodeToolStats toolcounts =
    E.object
        [ ( "tool-video", E.int toolcounts.video )
        , ( "tool-audio", E.int toolcounts.audio )
        , ( "tool-text", E.int toolcounts.text )
        , ( "tool-html", E.int toolcounts.html )
        , ( "tool-pdf", E.int toolcounts.pdf )
        , ( "tool-shape", E.int toolcounts.shape )
        , ( "tool-slideshow", E.int toolcounts.slideshow )
        , ( "tool-picture", E.int toolcounts.image )
        ]


encodeMetrics : Metrics -> E.Value
encodeMetrics metrics =
    E.object
        [ ( "alignment_score", E.float metrics.alignmentScore )
        , ( "spacing_score", E.float metrics.spacingScore )
        , ( "size_uniformity_score", E.float metrics.sizeUniformityScore )
        , ( "overlap_percentage", E.float metrics.overlapPercentage )
        , ( "white_space_percentage", E.float metrics.whiteSpacePercentage )
        , ( "horizontal_vertical_ratio", E.float metrics.horizontalVerticalRatio )
        , ( "overall_regular_score", E.float metrics.overallRegularScore )
        ]



-- type alias LinkCounts =
--     { broken : Int
--     , external : Int
--     , other_expositions : Int
--     , same_exposition : Int
--     , references : Int
--     }


encodeLinkCounts : LinkCounts -> E.Value
encodeLinkCounts linkCounts =
    E.object
        [ ( "broken", E.int linkCounts.broken )
        , ( "external", E.int linkCounts.external )
        , ( "other_expositions", E.int linkCounts.other_expositions )
        , ( "same_exposition", E.int linkCounts.same_exposition )
        , ( "references", E.int linkCounts.references )
        ]


decodeLinkCounts : D.Decoder LinkCounts
decodeLinkCounts =
    D.map5 LinkCounts
        (D.succeed 0)
        -- todo implement on backend
        (D.field "external" D.int)
        (D.field "other_expositions" D.int)
        (D.field "same_exposition" D.int)
        (D.field "references" D.int)



-- TODO implement full encoder


encodePageIdAndType : List ( Int, PageType ) -> E.Value
encodePageIdAndType lst =
    let
        props =
            lst
                |> List.map
                    (\( id, pt ) ->
                        let
                            strId =
                                String.fromInt id

                            value =
                                E.string (Expo.stringOfPageType pt)
                        in
                        ( strId, value )
                    )
    in
    E.object props



-- encodePageType : PageType -> E.Value
-- encodePageType pt =
--     pt |> stringOfPageType |> E.string


encodeFormat : Format -> E.Value
encodeFormat fm =
    let
        mDefaultPageFormat =
            fm.defaultPageFormat |> Maybe.map (\dfpf -> ( "defaultPageFormat", encodePageType dfpf ))

        encodedPerPage : E.Value
        encodedPerPage =
            fm.pageFormats |> Dict.toList |> encodePageIdAndType
    in
    E.object
        (prependMaybe mDefaultPageFormat
            [ ( "pageFormats", encodedPerPage )
            ]
        )


decodeFormat : D.Decoder Format
decodeFormat =
    D.map2
        (\dpage others -> { defaultPageFormat = dpage, pageFormats = others })
        (D.maybe (D.field "defaultPageFormat" decodePageType))
        (D.field "pageFormats" (dict2 D.int decodePageType))


encodePageType : PageType -> E.Value
encodePageType pt =
    E.string (Expo.stringOfPageType pt)


dict2 : D.Decoder comparable -> D.Decoder v -> D.Decoder (Dict.Dict comparable v)
dict2 keyDecoder valueDecoder =
    D.keyValuePairs valueDecoder
        |> D.andThen (decodeDictFromTuples keyDecoder)


{-| Helper function for dict
-}
decodeDictFromTuples : D.Decoder comparable -> List ( String, v ) -> D.Decoder (Dict.Dict comparable v)
decodeDictFromTuples keyDecoder tuples =
    case tuples of
        [] ->
            D.succeed Dict.empty

        ( strKey, value ) :: rest ->
            case D.decodeString keyDecoder strKey of
                Ok key ->
                    decodeDictFromTuples keyDecoder rest
                        |> D.andThen (Dict.insert key value >> D.succeed)

                Err error ->
                    D.fail (D.errorToString error)


prependMaybe : Maybe a -> List a -> List a
prependMaybe x xs =
    case x of
        Nothing ->
            xs

        Just some ->
            some :: xs



--- this is for url use:


tt_string_short : ToolType -> String
tt_string_short tt =
    case tt of
        VideoTool ->
            "vid"

        AudioTool ->
            "aud"

        HtmlTool ->
            "htm"

        PdfTool ->
            "pdf"

        ShapeTool ->
            "shp"

        SlideshowTool ->
            "sld"

        PictureTool ->
            "img"

        SimpleTextTool ->
            "txt"

        EmbedTool ->
            "emb"

        NoteTool ->
            "not"


tt_of_string : String -> Result String ToolType
tt_of_string s =
    case s of
        "vid" ->
            Ok VideoTool

        "aud" ->
            Ok AudioTool

        "htm" ->
            Ok HtmlTool

        "pdf" ->
            Ok PdfTool

        "shp" ->
            Ok ShapeTool

        "sld" ->
            Ok SlideshowTool

        "img" ->
            Ok PictureTool

        "txt" ->
            Ok SimpleTextTool

        "emb" ->
            Ok EmbedTool

        "not" ->
            Ok NoteTool

        _ ->
            Err ("didn't match a tool type :" ++ s)


encodeString : StatsSort -> String
encodeString ss =
    -- this function takes a ToolType and turns it into three letter string:
    case ss of
        MostTools tt ->
            "mostTools" ++ "_" ++ tt_string_short tt

        Overlap ->
            "overlap"

        MostPages ->
            "mostpages"

        OveralTools ->
            "overaltools"

        WhiteSpace ->
            "whitespace"

        Spacing ->
            "spacing"

        Uniformity ->
            "uniform"

        Alignment ->
            "align"

        Horizontal ->
            "horizontal"

        MostLinks linktype ->
            "mostLinks" ++ "_" ++ link_type_to_string linktype



-- for use in urls


link_type_from_string : String -> Result String StatsSort
link_type_from_string str =
    case str of
        "extlink" ->
            Ok (MostLinks ExternalLink)

        "intlink" ->
            Ok (MostLinks NavLink)

        "brolink" ->
            Ok (MostLinks Broken)

        "otherexplink" ->
            Ok (MostLinks OtherExpoLink)

        "reflink" ->
            Ok (MostLinks ReferenceLink)

        _ ->
            Err ("unkown link type " ++ str)


link_type_to_string : LinkType -> String
link_type_to_string lt =
    case lt of
        ExternalLink ->
            "extlink"

        NavLink ->
            "intlink"

        ReferenceLink ->
            "reflink"

        Broken ->
            "brolink"

        OtherExpoLink ->
            "otherexplink"


decodeString : String -> Result String StatsSort
decodeString str =
    case String.split "_" str of
        [ "mostTools", rest ] ->
            tt_of_string rest
                |> Result.map MostTools

        [ "mostLinks", rest ] ->
            link_type_from_string rest

        [ "overlap" ] ->
            Ok Overlap

        [ "mostpages" ] ->
            Ok MostPages

        [ "overaltools" ] ->
            Ok OveralTools

        [ "uniform" ] ->
            Ok Uniformity

        [ "align" ] ->
            Ok Alignment

        [ "horizontal" ] ->
            Ok Horizontal

        _ ->
            Err <| "expected mostTools or it is not a two item list\n" ++ str


viewStat : Stats -> String
viewStat st =
    st.format |> displayFormat MinDetail


viewToolCount : ToolCounts -> Tools.ToolType -> Int
viewToolCount counts tt =
    case tt of
        VideoTool ->
            counts.video

        AudioTool ->
            counts.audio

        HtmlTool ->
            counts.html

        PdfTool ->
            counts.pdf

        ShapeTool ->
            counts.shape

        SlideshowTool ->
            counts.slideshow

        PictureTool ->
            counts.image

        SimpleTextTool ->
            counts.text

        EmbedTool ->
            0

        -- Assuming no embed tool count in ToolCounts
        NoteTool ->
            0


type StatField
    = StatQuantity Int
    | StatFloat Float
    | StatUrl String
    | StatMissing


renderStatField : StatField -> Element.Element msg
renderStatField statfield =
    case statfield of
        StatQuantity i ->
            Element.text (String.fromInt i)

        StatUrl s ->
            Element.link [] { url = s, label = Element.text s }

        StatFloat f ->
            Element.text (String.fromFloat f)

        StatMissing ->
            Element.text "not available"


viewStatsAsTable : (String -> String) -> Stats -> Element.Element msg
viewStatsAsTable localizeUrl stats =
    let
        labelWidth =
            Element.px 125

        valueWidth =
            Element.px 100

        row : Int -> String -> StatField -> Element.Element msg
        row index label statfield =
            let
                bgColor =
                    if modBy 2 index == 0 then
                        Element.Background.color (Element.rgb255 225 225 225)
                        -- Light gray

                    else
                        Element.Background.color (Element.rgb255 255 255 255)
            in
            Element.row
                ([ Element.width (Element.px 450)
                 , Element.spacingXY 0 0
                 , Element.paddingXY 10 5
                 , bgColor
                 ]
                    ++ (if index == 0 then
                            []

                        else
                            []
                       )
                )
                [ Element.el [ Element.width labelWidth ] (Element.text label)
                , Element.el [ Element.width valueWidth ] (renderStatField statfield)
                ]

        overalStats =
            [ ( "ID", StatQuantity stats.id )
            , ( "Number of Pages", StatQuantity stats.numberOfPages )
            , ( "Default Page", StatUrl stats.defaultPage )
            , ( "Total Number of Tools", StatQuantity stats.totalNumberOfTools )
            , ( "Video Tools", StatQuantity stats.toolStats.video )
            , ( "Audio Tools", StatQuantity stats.toolStats.audio )
            , ( "Text Tools", StatQuantity stats.toolStats.text )
            , ( "HTML Tools", StatQuantity stats.toolStats.html )
            , ( "PDF Tools", StatQuantity stats.toolStats.pdf )
            , ( "Shape Tools", StatQuantity stats.toolStats.shape )
            , ( "Slideshow Tools", StatQuantity stats.toolStats.slideshow )
            , ( "Image Tools", StatQuantity stats.toolStats.image )
            ]

        metricStats =
            case stats.metrics of
                Just m ->
                    [ ( "Alignment Score", StatFloat m.alignmentScore )
                    , ( "Spacing Score", StatFloat m.spacingScore )
                    , ( "Size Uniformity Score", StatFloat m.sizeUniformityScore )
                    , ( "Overlap Percentage", StatFloat m.overlapPercentage )
                    , ( "White Space Percentage", StatFloat m.whiteSpacePercentage )
                    , ( "Horizontal Vertical Ratio", StatFloat m.horizontalVerticalRatio )
                    , ( "Overall Regular Score", StatFloat m.overallRegularScore )
                    ]

                Nothing ->
                    [ ( "Metrics", StatMissing ) ]

        linkStats =
            case stats.linkCounts of
                Just lc ->
                    [ ( "External Links", StatQuantity lc.external )
                    , ( "Other Expositions Links", StatQuantity lc.other_expositions )
                    , ( "Internal Links", StatQuantity lc.same_exposition )
                    , ( "Reference Links", StatQuantity lc.references )
                    , ( "Broken Links", StatQuantity lc.broken )
                    ]

                Nothing ->
                    [ ( "Link Counts", StatMissing ) ]

        column_with_data title_element labels_and_values =
            Element.column
                RCStyles.tablestyling
                (Element.el
                    [ Element.Background.color (Element.rgb255 192 192 192)
                    , Font.size 12
                    , Element.paddingXY 5 5
                    , Font.bold
                    , Element.width Element.fill
                    ]
                    title_element
                    :: List.indexedMap (\i ( label, value ) -> row i label value) labels_and_values
                )

        hyperlinkDetailUrl = 
             ("https://map.rcdata.org/rcjson/expo/" ++ String.fromInt stats.id ++ "/hyperlinks")

        hyperlinkLabelLink =
            Element.row [ Element.spacingXY 0 6, Element.width Element.fill ]
                [ Element.el [ Element.width Element.shrink ] (Element.text "hyperlinks")
                , Element.el [ Element.width Element.fill ] (Element.text "")
                , Element.link
                    [ Element.width Element.shrink, Font.color (Element.rgb255 0 0 128) ]
                    { label = Element.text "view detail 🔍"
                    , url = hyperlinkDetailUrl
                    }
                ]
    in
    Element.column []
        [ Element.el
            [ Font.italic
            , Font.family [ Font.monospace ]
            , Font.size 16
            , Element.paddingEach { top = 0, left = 0, bottom = 20, right = 15 }
            ]
            (Element.text "Exposition Metrics:")
        , column_with_data (Element.text "overal") overalStats
        , column_with_data (Element.text "metrics") metricStats
        , column_with_data hyperlinkLabelLink linkStats
        ]
