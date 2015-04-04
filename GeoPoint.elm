module GeoPoint (GeoPoint, TileOffset) where

type alias GeoPoint = { lat: Float, lon: Float }
type alias TileOffset = { index: Int, pixel: Int }
