module Types (GeoPoint, Model, Position, TileOffset, TileRenderer, Tile, Zoom(..)) where

import Graphics.Element (Element)

type Zoom = Zoom Int
type alias GeoPoint = { lat: Float, lon: Float }
type alias Tile = { coordinate : (Int, Int) }
type alias Position = { pixels : (Int, Int) }
type alias TileOffset = { tile: Tile, position: Position }
type alias TileRenderer = Zoom -> Int -> Tile -> Element
type alias Model = {
      tileSize : Int,
      centre : GeoPoint,
      zoom : Zoom,
      converter : Zoom -> GeoPoint -> TileOffset,
      mouseState : (Bool, (Int, Int)),
      renderer : TileRenderer
}

