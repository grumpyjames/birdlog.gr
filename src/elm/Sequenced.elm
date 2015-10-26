module Sequenced ( Recording(..)
                 , Sequenced
                 , consolidate
                 , matches ) where
    
import Dict as D
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

matches : Int -> a -> Sequenced (Recording a) -> Bool
matches sequence item = 
    \sr -> sr.sequence == sequence &&
           case sr.item of
             New n -> n == item
             Replace _ r -> r == item
             otherwise -> False
 