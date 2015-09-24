module Types (GeoPoint,
              Hdpi,
              Locator,
              Position,
              TileOffset,
              TileSource,
              TileUrl,
              Tile,
              Zoom(..)) where

import Time exposing (Time)


type alias GeoPoint = 
    { 
      lat: Float
    , lon: Float 
    }

type alias Tile = 
    { 
      coordinate : (Int, Int)
    }

type alias Position = 
    { 
      pixels : (Int, Int)
    }

type alias TileOffset = 
    { 
      tile: Tile
    , position: Position
    }

type alias Hdpi = Bool
type alias Locator = Float -> GeoPoint -> TileOffset
type alias TileUrl = Int -> Tile -> String

type Zoom = Constant Int
          | Between Int Int Float

type alias TileSource =
    {
      tileSize : Int
    , locate: Locator
    , tileUrl: TileUrl
    }


