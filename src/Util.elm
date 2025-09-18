module Util exposing (..)

embed : String -> String -> String
embed outside inside = 
    outside ++ inside ++ outside