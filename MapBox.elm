module MapBox (mapBox) where

import CommonLocator exposing (common)
import Types exposing (Hdpi, Tile, TileSource, Zoom)

mapBox : Hdpi -> String -> String -> TileSource
mapBox hdpi identifier token = 
    let tileSize = if hdpi then 512 else 256
    in TileSource tileSize (common tileSize) (mapBoxUrl hdpi identifier token)

mapBoxUrl : Hdpi -> String -> String -> Int -> Tile -> String
mapBoxUrl hdpi id token z t =
    let (x, y) = t.coordinate
        wrap z c = c % (2 ^ z)
        extra = if hdpi then "@2x" else "" 
    in "https://api.tiles.mapbox.com/v4/" ++ id ++ "/" ++ (toString z) ++ "/" ++ (toString (wrap z x)) ++ "/" ++ (toString y) ++ extra ++ ".png?access_token=" ++ token 