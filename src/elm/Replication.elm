module Replication (postRecords) where

import Model exposing (Events(..), Recording(..), Sequenced, Sighting)
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
    let encode rs = Http.string <| JS.encode 0 <| JS.list <| L.map asJson rs 
    in case rs of
      [] -> ReplicationPacket -1 Http.empty
      (x :: xs) -> ReplicationPacket x.sequence (encode rs)

asJson : (Sequenced (Recording Sighting)) -> JS.Value
asJson r = 
    let nora typ seq item =
        JS.object [ ("sequence", (JS.int seq))
                  , ("type", typ)
                  , ("item", item) 
                  ]
    in case r.item of 
      New s -> 
          nora typeNew r.sequence <| sghtJson s
      Replace refSeq s -> 
          nora (typeRef "Replace" refSeq) r.sequence <| sghtJson s
      Delete refSeq -> 
          nora (typeRef "Delete" refSeq) r.sequence <| JS.object []

typeNew : JS.Value
typeNew = JS.object [ ("name", JS.string "New") ]

typeRef : String -> Int -> JS.Value
typeRef n seq = JS.object [ ("name", JS.string n)
                          , ("refersTo", JS.object [("sequence", JS.int seq)])
                          ]

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

