module Styles (Style, absolute, noDisplay, px, position, dimensions, zeroMargin) where

type alias Style = List (String, String)

px : Int -> String
px n = (toString n) ++ "px"   

absolute : Style
absolute = [("position", "absolute")]

position : (Int, Int) -> Style
position (x, y) = [("top", px y), ("left", px x)]

dimensions : (Int, Int) -> Style
dimensions (width, height) = [("width", px width), ("height", px height)]

zeroMargin : Style
zeroMargin = [("padding", px 0), ("margin", px 0)]

noDisplay : Style
noDisplay = [("display", "none")]
