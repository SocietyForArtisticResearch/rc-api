module Tools exposing (ToolType(..), displayToolType, fromRClass, toRClass)

import Expo exposing (Media(..))
import Html exposing (Html)


type ToolType
    = SimpleTextTool
    | HtmlTool
    | VideoTool
    | PictureTool
    | SlideshowTool
    | PdfTool
    | ShapeTool
    | EmbedTool
    | AudioTool
    | NoteTool


toRClass : ToolType -> String
toRClass tt =
    case tt of
        SimpleTextTool ->
            "tool-simple-text"

        HtmlTool ->
            "tool-text"

        VideoTool ->
            "tool-video"

        PictureTool ->
            "tool-picture"

        SlideshowTool ->
            "tool-slideshow"

        PdfTool ->
            "tool-pdf"

        ShapeTool ->
            "tool-shape"

        EmbedTool ->
            "tool-embed"

        AudioTool ->
            "tool-audio"

        NoteTool ->
            "tool-note"

fromUsagesString : String -> ToolType 
fromUsagesString str= 
    case str of 
        "picture" -> PictureTool

        "video" -> VideoTool

        "audio" -> AudioTool

        "slideshow" -> SlideshowTool

        _ -> PictureTool -- TODO add more options!



fromRClass : String -> Maybe ToolType
fromRClass rClass =
    case rClass of
        "tool-simple-text" ->
            Just SimpleTextTool

        "tool-text" ->
            Just HtmlTool

        "tool-video" ->
            Just VideoTool

        "tool-picture" ->
            Just PictureTool

        "tool-slideshow" ->
            Just SlideshowTool

        "tool-pdf" ->
            Just PdfTool

        "tool-shape" ->
            Just ShapeTool

        "tool-embed" ->
            Just EmbedTool

        "tool-audio" ->
            Just AudioTool

        "tool-note" ->
            Just NoteTool

        _ ->
            Nothing


displayToolType : ToolType -> String
displayToolType tt =
    (case tt of
        SimpleTextTool ->
            "simple text tool"

        HtmlTool ->
            "html tool"

        VideoTool ->
            "video tool"

        PictureTool ->
            "image tool"

        SlideshowTool ->
            "slideshow tool"

        PdfTool ->
            "pdf tool"

        ShapeTool ->
            "shape tool"

        EmbedTool ->
            "embed tool"

        AudioTool ->
            "audio tool"

        NoteTool ->
            "note tool"
    )
        ++ "s"
