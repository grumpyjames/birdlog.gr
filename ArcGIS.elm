module ArcGIS (arcGIS) where

import CommonLocator (common)
import Types (Tile, TileSource, Zoom(..))

import Graphics.Element (Element, image)

arcGIS : TileSource
arcGIS = TileSource 256 (common 256) render

render : Zoom -> Int -> Tile -> Element
render zoom size tile = image size size <| arcGISUrl zoom tile.coordinate

arcGISBase = "http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/"

arcGISUrl : Zoom -> (Int, Int) -> String
arcGISUrl zoom (x,y) =
    let wrap z c = c % (2 ^ z) 
    in case zoom of Zoom z -> arcGISBase ++ (toString z) ++ "/" ++ (toString y) ++ "/" ++ (toString (wrap z x)) ++ ".png"