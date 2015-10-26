module Sequenced ( Recording(..)
                 , Sequenced
                 , consolidate
                 , encoder
                 , fold
                 , matches ) where
    
import Dict as D
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
        case r.item of
          New s -> D.insert r.sequence (Sequenced r.sequence s) d
          Replace seq s -> D.insert r.sequence (Sequenced r.sequence s) <| D.remove seq d
          Delete seq -> D.remove seq d
    in D.values <| L.foldr f D.empty rs

encoder : (a -> JE.Value) -> Sequenced (Recording a) -> JE.Value
encoder valEnc s =
    let nora typ seq item =
        JE.object [ ("sequence", (JE.int seq))
                  , ("type", typ)
                  , ("item", item) 
                  ]
    in case s.item of 
      New n -> 
          nora typeNew s.sequence <| valEnc n
      Replace refSeq r -> 
          nora (typeRef "Replace" refSeq) s.sequence <| valEnc r
      Delete refSeq -> 
          nora (typeRef "Delete" refSeq) s.sequence <| JE.object []

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
 