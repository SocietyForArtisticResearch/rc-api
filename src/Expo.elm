module Expo exposing (..)

import Research exposing (ExpositionID)



{-
   An RC exposition, is a collection of pages.
   there is also a table of content (toc) and some metadata.
-}


type Exposition
    = Exposition
        { pages : List Page
        , toc : Toc
        , meta : Meta
        }


{-| Within a page, there is a list of tools, that construct the content of the page.
The page settings set some general meta and style properties of the page.
-}
type Page
    = Page
        { pageType : PageType
        , content : Content
        , settings : PageSettings
        }


{-| There are three types of pages, differentiated by the way they are structured spatially.

  - Graphical (x,y pixel position)
  - Block (int row column)
  - Text (markdown character offset, but for now maybe better left untouched as we do not have a way to get markdown source)

-}
type PageType
    = Graphical
    | Text
    | Block


{-| Pages are constructed using tools
-}
type alias Content =
    List Tool


{-| A tool is a container of content
-}
type Tool
    = Tool
        { content : ToolContent
        , name : String
        , style : List Style
        , position : Dimensions
        }


{-| Content is either plain text, html or a file url in the RC media database
-}
type ToolContent
    = HtmlTxt String
    | Txt String
    | MediaUrl String


{-| Tools may be styled using a specific subset of CSS properties
-}
type
    Style
    -- padding, border, background color etc..
    = Style String String


{-| An exposition has a TOC, that provides a kind of navigation menu of the exposition
-}
type
    Toc
    -- table of contents
    = Toc (List TocEntry)


{-| The toc entries are either links to a page, a tool within a page, or a link to position within a page
-}
type TocEntry
    = PageLink PageID
    | ToolLink PageID String ToolId
    | PositionLink PageID String { x : Int, y : Int }


{-| The tool unique identifier
-}
type ToolId
    = ToolId Int


{-| Meta data of the exposition (TODO link this to the Research.elm Research type alias)
-}
type Meta
    = Meta Research.Res


{-| The unique ID of the page
-}
type PageID
    = PageID Int


{-| Title and id of the page
-}
type PageSettings
    = PageSettings { title : String, pageID : PageID }


type Media
    = Video
    | Audio
    | Image ImageType
    | PDF


type ImageType
    = PNG
    | JPG
    | SVG


type Dimensions
    = CartDim { x : Int, y : Int, w : Int, h : Int }
    | BlockPosition { row : Int, col : Int }
