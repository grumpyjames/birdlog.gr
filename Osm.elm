module Osm (osm) where

import Graphics.Element (Element, image)
import Tile (..)

osm : Render
osm size tile = image size size <| osmUrl 5 tile.point

osmUrl : Int -> (Int, Int) -> String
osmUrl zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString (x+10)) ++ "/" ++ (toString ((1-y)+10)) ++ ".png"

-- conversions (unused as yet)
log = logBase e
tiley2lat y z = 
    let n = pi - 2.0 * pi * y / (2.0 ^ z)
    in 180.0 / pi * atan ( 0.5 * ( (e ^ n) - (e ^ (-n) ) ) )                
long2tilex lon z = floor((lon + 180.0) / 360.0 * (2.0 ^ z)) 
lat2tiley lat z = floor((1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ z))
tilex2long x z = x / (2.0 ^ z) * 360.0 - 180