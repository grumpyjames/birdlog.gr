module Results ( fold
               , mapError)
    where

import Result exposing (Result(..))

mapError : (e1 -> e2) -> Result e1 a -> Result e2 a
mapError g r = 
    case r of
      Ok o -> Ok o
      Err e -> Err (g e)

fold : (e -> c) -> (o -> c) -> Result e o -> c
fold f g r = 
    case r of
      Ok o -> g o
      Err e -> f e
