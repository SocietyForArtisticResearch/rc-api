module KeywordString exposing (KeywordString(..), fromString, toString)


type KeywordString
    = KeywordString String


toString : KeywordString -> String
toString (KeywordString k) =
    k


fromString : String -> KeywordString
fromString str =
    KeywordString (str |> String.toLower |> String.trim)
