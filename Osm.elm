module Osm (centeredOn, osm) where

import Graphics.Element (Element, image)
import Tile (..)

-- FIXME: primitive obsession!
centeredOn : Int -> Float -> Float -> Element
centeredOn zoom lat lon = 
    let tx = lon2tilex lon zoom
        ty = lat2tiley lat zoom
    in image 256 256 <| osmUrl2 zoom (tx, ty)

osm : Render
osm size tile = image size size <| osmUrl 5 tile.point

osmUrl2 : Int -> (Int, Int) -> String
osmUrl2 zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString x) ++ "/" ++ (toString y) ++ ".png"

osmUrl : Int -> (Int, Int) -> String
osmUrl zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString (x+10)) ++ "/" ++ (toString ((1-y)+10)) ++ ".png"

-- conversions (unused as yet)
log = logBase e
tiley2lat y z = 
    let n = pi - 2.0 * pi * y / (2.0 ^ z)
    in 180.0 / pi * atan ( 0.5 * ( (e ^ n) - (e ^ (-n) ) ) )                

lon2tilex : Float -> Int -> Int
lon2tilex lon z = floor((lon + 180.0) / 360.0 * (2.0 ^ (toFloat z))) 

lat2tiley : Float -> Int -> Int
lat2tiley lat z = floor ((1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ (toFloat z)))

tilex2long x z = x / (2.0 ^ z) * 360.0 - 180