module Osm (GeoPoint, centeredOn, convert, osm, simpleOsm, tileSize) where

import Graphics.Element (Element, image)
import Tile (..)
import Tuple (mapT)

type alias TileOffset = { index: Int, pixel: Int }
type alias GeoPoint = { lat: Float, lon: Float }

tileSize = 256

convert : Int -> GeoPoint -> (TileOffset, TileOffset)
convert zoom geopt = mapT toOffset (lon2tilex zoom geopt.lon, lat2tiley zoom geopt.lat)

centeredOn : Int -> GeoPoint -> Element
centeredOn zoom geopt = 
    let tx = toOffset <| lon2tilex zoom geopt.lon
        ty = toOffset <| lat2tiley zoom geopt.lat
    in image tileSize tileSize <| osmUrl2 zoom (tx.index, ty.index)

osm : Render
osm zoom size tile = image size size <| osmUrl zoom tile

simpleOsm zoom tc = image tileSize tileSize <| osmUrl2 zoom tc

osmUrl2 : Int -> (Int, Int) -> String
osmUrl2 zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString x) ++ "/" ++ (toString y) ++ ".png"

osmUrl : Int -> (Int, Int) -> String
osmUrl zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString (x+10)) ++ "/" ++ (toString ((1-y)+10)) ++ ".png"

-- conversions
log = logBase e
tiley2lat y z = 
    let n = pi - 2.0 * pi * y / (2.0 ^ z)
    in 180.0 / pi * atan ( 0.5 * ( (e ^ n) - (e ^ (-n) ) ) )                

toOffset : Float -> TileOffset
toOffset f = 
    let index = floor f
        pixel = floor ((f - (toFloat index)) * tileSize)
    in { index = index, pixel = pixel }  

lon2tilex : Int -> Float -> Float
lon2tilex z lon = (lon + 180.0) / 360.0 * (2.0 ^ (toFloat z)) 

lat2tiley : Int -> Float -> Float
lat2tiley z lat = (1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ (toFloat z))

tilex2long x z = x / (2.0 ^ z) * 360.0 - 180