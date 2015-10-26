module Sighting ( Sighting
                , decoder
                , encoder ) where

import Types exposing (GeoPoint)

import Json.Decode as JD exposing ((:=))
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

decoder : JD.Decoder Sighting
decoder =
    JD.object4 Sighting ("count" := JD.int) ("species" := JD.string) ("location" := geoDecoder) ("time" := JD.float)

encoder : Sighting -> JE.Value
encoder s = 
    JE.object [ ("count", JE.int s.count)
              , ("species", JE.string s.species)
              , ("location", geoEncoder s.location)
              , ("time", JE.float s.time)
              ]

-- could move these to types?
geoDecoder : JD.Decoder GeoPoint
geoDecoder = 
    JD.object2 GeoPoint ("lat" := JD.float) ("lon" := JD.float)

geoEncoder : GeoPoint -> JE.Value
geoEncoder gp = JE.object [ ("lat", JE.float gp.lat)
                          , ("lon", JE.float gp.lon)
                          ]


