module Arrays (cartesian, main) where

import Array exposing (..)
import Graphics.Element exposing (show)
import Native.Arrays

cartesian = Native.Arrays.cartesian

main = show <| cartesian (,) (fromList [1,2,3]) (fromList [2,3,4]) 