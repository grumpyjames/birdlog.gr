module Arrays (cartesian) where

import Array exposing (..)
import Native.Arrays

cartesian : (a -> b -> c) -> List a -> List b -> Array (Array c)
cartesian f xs ys = Native.Arrays.cartesian f (fromList xs) (fromList ys)