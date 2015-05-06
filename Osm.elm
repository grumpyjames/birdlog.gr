module Osm (openStreetMap, simpleOsm) where

import CommonLocator exposing (common)
import Types exposing (GeoPoint, Locator, Position, Tile, TileOffset, TileSource, TileUrl, Zoom(..))

import Graphics.Element exposing (image)

tileSize = 256

locate = common tileSize

openStreetMap : TileSource
openStreetMap = TileSource tileSize locate osmUrl

simpleOsm zoom tc = image tileSize tileSize <| osmUrl zoom tc

osmUrl : Zoom -> Tile -> String
osmUrl zoom t =
    let (x, y) = t.coordinate
        wrap z c = c % (2 ^ z) 
    in case zoom of Zoom z -> "http://tile.openstreetmap.org/" ++ (toString z) ++ "/" ++ (toString (wrap z x)) ++ "/" ++ (toString y) ++ ".png"