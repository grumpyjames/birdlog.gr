module CommonLocator (common, tiley2lat, tilex2long, toPixels) where

import Tuple as T
import Types exposing (Locator, Position, Tile, TileOffset, Zoom)

-- This locator appears to work for most tile sources
common : Int -> Locator
common tileSize zoom geopt =
    let index f = floor f
        pixel f = floor <| (f - (toFloat (index f))) * (toFloat tileSize)
        tile f1 f2 = Tile (index f1, index f2)
        position f1 f2 = Position (pixel f1, pixel f2)
        toOffset tX tY = TileOffset (tile tX tY) (position tX tY) 
    in toOffset (lon2tilex zoom geopt.lon) (lat2tiley zoom geopt.lat)

log = logBase e

lon2tilex : Float -> Float -> Float
lon2tilex z lon = 
    (lon + 180.0) / 360.0 * (2.0 ^ (toFloat (floor z))) 

lat2tiley : Float -> Float -> Float
lat2tiley z lat = 
    (1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ (toFloat (floor z)))

toPixels : Int -> TileOffset -> (Int, Int)
toPixels tileSize tileOff = 
    let fromTile = T.map ((*) tileSize) tileOff.tile.coordinate
        fromPixels = tileOff.position.pixels
    in fromTile `T.add` fromPixels

-- currently unused
tiley2lat y z = 
    let n = pi - 2.0 * pi * y / (2.0 ^ z)
    in 180.0 / pi * atan ( 0.5 * ( (e ^ n) - (e ^ (-n) ) ) )                

tilex2long x z = x / (2.0 ^ z) * 360.0 - 180