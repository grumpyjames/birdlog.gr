module Types (GeoPoint,
              Locator,
              Model,
              Position,
              Renderer,              
              TileOffset,
              TileSource,
              Tile,
              Zoom(..)) where

import Graphics.Element (Element)

type Zoom = Zoom Int
type alias GeoPoint = { lat: Float, lon: Float }
type alias Tile = { coordinate : (Int, Int) }
type alias Position = { pixels : (Int, Int) }
type alias TileOffset = { tile: Tile, position: Position }
type alias Locator = Zoom -> GeoPoint -> TileOffset
type alias Renderer = Zoom -> Int -> Tile -> Element
type alias TileSource = {
      tileSize : Int,
      locate: Locator,
      render: Renderer
}
type alias Model = {
      centre : GeoPoint,
      zoom : Zoom,
      mouseState : (Bool, (Int, Int)),
      tileSource : TileSource
}

