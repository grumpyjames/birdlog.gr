module Osm (openStreetMap, simpleOsm) where

import CommonLocator (common)
import Types (GeoPoint, Locator, Position, Renderer, Tile, TileOffset, TileSource, Zoom(..))
import Tuple (mapT)

import Graphics.Element (Element, image)

tileSize = 256

locate = common tileSize

centeredOn : Zoom -> GeoPoint -> Element
centeredOn zoom geopt = 
    let os = locate zoom geopt
    in image tileSize tileSize <| osmUrl zoom os.tile.coordinate

openStreetMap : TileSource
openStreetMap = TileSource tileSize locate osm

osm : Renderer
osm zoom size tile = image size size <| osmUrl zoom tile.coordinate

simpleOsm zoom tc = image tileSize tileSize <| osmUrl zoom tc

osmUrl : Zoom -> (Int, Int) -> String
osmUrl zoom (x,y) =
    let wrap z c = c % (2 ^ z) 
    in case zoom of Zoom z -> "http://tile.openstreetmap.org/" ++ (toString z) ++ "/" ++ (toString (wrap z x)) ++ "/" ++ (toString y) ++ ".png"