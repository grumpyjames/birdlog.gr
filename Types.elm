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
      count: Int
    , name: String
    , location: GeoPoint
    }
type alias Model = {
      centre : GeoPoint,
      zoom : Zoom,
      mouseState : (Bool, (Int, Int)),
      tileSource : TileSource,
      clicked : Maybe (Int, Int),
      sighting : Sighting,
      progress : Bool
}