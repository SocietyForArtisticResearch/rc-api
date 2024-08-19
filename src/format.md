# Format

Open questions:

* What would be a suitable storage - transport format for RC expositions
* How to structure tools, should they have different types, or all be the same and just have different content?

* distribution of tools
* proximity
* histograms of types
* alignment / overlap 

type ToolContent =
  PictureTool
  VideoTool
  TextTool

type tool = 
  RCTool { type : ToolType, position : Position, meta : Meta }

type Content = 
  { box : Dimension 
  , content : (media url | text | etc.. )

  
* Store the data as a database, or just a big set of JSON?
