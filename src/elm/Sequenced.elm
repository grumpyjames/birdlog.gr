module Sequenced ( Recording(..)
                 , Sequenced
                 , consolidate
                 , decoder
                 , encoder
                 , fold
                 , matches ) where

import Maybes
    
import Dict as D
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import List as L

type alias Sequenced a = 
    { sequence: Int
    , item : a
    }

type Recording a = New a
                 | Replace Int a
                 | Delete Int

consolidate : List (Sequenced (Recording a)) -> List (Sequenced a)
consolidate rs = 
    let f r d =
        fold (\s i -> D.insert s (Sequenced s i) d)
             (\s rs i -> D.insert s (Sequenced s i) <| D.remove rs d)
             (\s rs -> D.remove rs d)
             r
    in D.values <| L.foldr f D.empty rs

decoder : (JD.Decoder a) -> (JD.Decoder (Sequenced (Recording a)))
decoder valDecoder = 
   JD.customDecoder (JD.object3 AlmostRecord
          ("sequence" := JD.int)
          (JD.maybe <| JD.at ["type", "refersTo", "sequence"] JD.int)
          ("record" := JD.maybe valDecoder)) parseRecording 

parseRecording : (AlmostRecord a) -> Result String (Sequenced (Recording a))
parseRecording ar = 
    let repOrNew a = Result.Ok <| (Maybes.fold Replace New ar.replaces) a
        del = Maybes.fold (Result.Ok << Delete) (Result.Err "impossible") ar.replaces
    in Result.map (Sequenced ar.sequence) <| Maybes.fold repOrNew del ar.record 
 
type alias AlmostRecord a = 
    { sequence: Int
    , replaces: Maybe Int
    , record: Maybe a
    }

encoder : (a -> JE.Value) -> Sequenced (Recording a) -> JE.Value
encoder valEnc sequenced =
    let nora typ seq item =
        JE.object [ ("sequence", (JE.int seq))
                  , ("type", typ)
                  , ("item", item) 
                  ]
    in 
      fold (\s i -> nora typeNew s (valEnc i))
           (\s rs i -> nora (typeRef "Replace" rs) s (valEnc i))
           (\s rs -> nora (typeRef "Delete" rs) s <| JE.object [])
           sequenced

typeNew : JE.Value
typeNew = JE.object [ ("name", JE.string "New") ]

typeRef : String -> Int -> JE.Value
typeRef n seq = JE.object [ ("name", JE.string n)
                          , ("refersTo", JE.object [("sequence", JE.int seq)])
                          ]


fold : (Int -> a -> b) -> (Int -> Int -> a -> b) -> (Int -> Int -> b) -> (Sequenced (Recording a)) -> b
fold onNew onReplace onDelete s =
    case s.item of
      New n -> onNew s.sequence n
      Replace seq r -> onReplace s.sequence seq r
      Delete seq -> onDelete s.sequence seq

matches : Int -> a -> Sequenced (Recording a) -> Bool
matches sequence item = 
    \sr -> sr.sequence == sequence &&
           case sr.item of
             New n -> n == item
             Replace _ r -> r == item
             otherwise -> False
 