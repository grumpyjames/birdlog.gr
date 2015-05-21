module TouchDemo (main) where

import Graphics.Element exposing (show)
import List exposing ((::))
import Signal exposing (foldp, map)
import Touch exposing (touches)

main = map show (foldp (::) [] touches)