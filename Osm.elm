module Osm (centeredOn, convert, osm, simpleOsm, tileSize) where

import Graphics.Element (Element, image)
import GeoPoint (GeoPoint)
import Tile (TileRenderer, Zoom(..))
import Tuple (mapT)

type alias TileOffset = { index: Int, pixel: Int }

tileSize = 256

convert : Zoom -> GeoPoint -> (TileOffset, TileOffset)
convert zoom geopt = mapT toOffset (lon2tilex zoom geopt.lon, lat2tiley zoom geopt.lat)

centeredOn : Zoom -> GeoPoint -> Element
centeredOn zoom geopt = 
    let tx = toOffset <| lon2tilex zoom geopt.lon
        ty = toOffset <| lat2tiley zoom geopt.lat
    in image tileSize tileSize <| osmUrl zoom (tx.index, ty.index)

osm : TileRenderer
osm zoom size tile = image size size <| osmUrl zoom tile

simpleOsm zoom tc = image tileSize tileSize <| osmUrl zoom tc

osmUrl : Zoom -> (Int, Int) -> String
osmUrl zoom (x,y) =
    let wrap z c = c % (2 ^ z) 
    in case zoom of Zoom z -> "http://tile.openstreetmap.org/" ++ (toString z) ++ "/" ++ (toString (wrap z x)) ++ "/" ++ (toString y) ++ ".png"

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

lon2tilex : Zoom -> Float -> Float
lon2tilex zoom lon = 
    case zoom of Zoom z -> (lon + 180.0) / 360.0 * (2.0 ^ (toFloat z)) 

lat2tiley : Zoom -> Float -> Float
lat2tiley zoom lat = 
    case zoom of Zoom z -> (1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ (toFloat z))

tilex2long x z = x / (2.0 ^ z) * 360.0 - 180