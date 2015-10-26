module Maybes (fold) where

import Maybe as M

fold : (a -> b) -> b -> Maybe a -> b
fold f d myb = M.withDefault d <| M.map f myb