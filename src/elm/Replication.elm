module Replication (postRecords) where

import Model exposing (Events(..))
import Sequenced exposing (Recording(..), Sequenced)

import Http
import Json.Decode as JD
import Json.Encode as JE
import List as L
import Signal as S
import Task exposing (Task)

type alias ReplicationPacket = { maxSequence: Int, body: Http.Body }

postRecords : S.Address (Events) -> (a -> JE.Value) -> List (Sequenced (Recording a)) -> Task Http.Error ()
postRecords addr encoder rs =
    let http p = Http.post (JD.succeed (HighWaterMark p.maxSequence)) "/api/records" p.body        
    in if (L.isEmpty rs) 
       then Task.succeed () 
       else http (pack encoder rs) 
                `Task.andThen` (S.send addr) 
                `Task.onError` (\err -> S.send addr ReplicationFailed)

pack : (a -> JE.Value) -> List (Sequenced (Recording a)) -> ReplicationPacket
pack valEncoder rs = 
    let encode rs = Http.string <| JE.encode 0 <| JE.list <| L.map (Sequenced.encoder valEncoder) rs 
    in case rs of
      [] -> ReplicationPacket -1 Http.empty
      (x :: xs) -> ReplicationPacket x.sequence (encode rs)