module Replication (postRecords) where

import Model exposing (Events(..), Sighting)
import Sequenced exposing (Recording(..), Sequenced, encoder)
import Types exposing (GeoPoint)

import Http
import Json.Decode as JD
import Json.Encode as JS
import List as L
import Signal as S
import Task exposing (Task)

type alias ReplicationPacket = { maxSequence: Int, body: Http.Body }

postRecords : S.Address (Events) -> List (Sequenced (Recording Sighting)) -> Task Http.Error ()
postRecords addr rs =
    let http p = Http.post (JD.succeed (HighWaterMark p.maxSequence)) "/api/records" p.body        
    in if (L.isEmpty rs) 
       then Task.succeed () 
       else http (pack rs) 
                `Task.andThen` (S.send addr) 
                `Task.onError` (\err -> S.send addr ReplicationFailed)

pack : List (Sequenced (Recording Sighting)) -> ReplicationPacket
pack rs = 
    let encode rs = Http.string <| JS.encode 0 <| JS.list <| L.map (encoder sghtJson) rs 
    in case rs of
      [] -> ReplicationPacket -1 Http.empty
      (x :: xs) -> ReplicationPacket x.sequence (encode rs)

sghtJson : Sighting -> JS.Value
sghtJson s = 
    JS.object [ ("count", JS.int s.count)
              , ("species", JS.string s.species)
              , ("location", geoJson s.location)
              , ("time", JS.float s.time)
              ]

geoJson : GeoPoint -> JS.Value
geoJson gp = JS.object [ ("lat", JS.float gp.lat)
                       , ("lon", JS.float gp.lon)
                       ]

