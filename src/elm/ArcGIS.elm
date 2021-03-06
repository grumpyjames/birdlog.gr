module ArcGIS (arcGIS) where

import CommonLocator exposing (common)
import Types exposing (Tile, TileSource, Zoom)

import Graphics.Element exposing (Element, image)

arcGIS : TileSource
arcGIS = TileSource 256 (common 256) arcGISUrl

arcGISBase = "http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/"

arcGISUrl : Int -> Tile -> String
arcGISUrl z t =
    let (x, y) = t.coordinate
        wrap z c = c % (2 ^ z) 
    in arcGISBase ++ (toString z) ++ "/" ++ (toString y) ++ "/" ++ (toString (wrap z x)) ++ ".png"