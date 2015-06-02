module MapBox (mapBox) where

import CommonLocator exposing (common)
import Types exposing (Tile, TileSource, Zoom)

tileSize = 256

locate = common tileSize

mapBox : String -> String -> TileSource
mapBox identifier token = TileSource tileSize (common tileSize) (mapBoxUrl identifier token)

mapBoxUrl : String -> String -> Zoom -> Tile -> String
mapBoxUrl id token z t =
    let (x, y) = t.coordinate
        wrap z c = c % (2 ^ (floor z)) 
    in "https://api.tiles.mapbox.com/v4/" ++ id ++ "/" ++ (toString (floor z)) ++ "/" ++ (toString (wrap z x)) ++ "/" ++ (toString y) ++ ".png?access_token=" ++ token 