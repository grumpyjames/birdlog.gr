module Types (GeoPoint,
              Hdpi,
              Locator,
              Model,
              Position,
              Sighting,
              TileOffset,
              TileSource,
              TileUrl,
              Tile,
              Zoom) where

import Time exposing (Time)

type alias Zoom = Float
type alias GeoPoint = { lat: Float, lon: Float }
type alias Tile = { coordinate : (Int, Int) }
type alias Position = { pixels : (Int, Int) }
type alias TileOffset = { tile: Tile, position: Position }
type alias Hdpi = Bool
type alias Locator = Zoom -> GeoPoint -> TileOffset
type alias TileUrl = Zoom -> Tile -> String
type alias TileSource = {
      tileSize : Int,
      locate: Locator,
      tileUrl: TileUrl
}
type alias Sighting =
    {
      count: Result String Int
    , name: String
    , location: GeoPoint
    -- millis
    , time : Time
    }
type alias Model = {
      hdpi : Bool,
      centre : GeoPoint,
      zoom : Zoom,
      mouseState : (Bool, (Int, Int)),
      tileSource : TileSource,
      clicked : (Time, Maybe (Int, Int)),
      sighting : Sighting,
      progress : Bool
}