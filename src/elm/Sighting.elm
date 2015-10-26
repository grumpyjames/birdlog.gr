module Sighting ( Sighting
                , encoder ) where

import Types exposing (GeoPoint)

import Json.Encode as JE
import Time exposing (Time)

type alias Sighting =
    {
      count: Int
    , species: String
    , location: GeoPoint
    -- millis
    , time : Time
    }

encoder : Sighting -> JE.Value
encoder s = 
    JE.object [ ("count", JE.int s.count)
              , ("species", JE.string s.species)
              , ("location", geoJson s.location)
              , ("time", JE.float s.time)
              ]

-- could move this to types? not sure.
geoJson : GeoPoint -> JE.Value
geoJson gp = JE.object [ ("lat", JE.float gp.lat)
                       , ("lon", JE.float gp.lon)
                       ]


