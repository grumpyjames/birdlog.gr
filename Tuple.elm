module Tuple (zero, merge, map, add, multiply, subtract) where

zero = (0, 0)

map : (a -> b) -> (a, a) -> (b, b)
map f (a1, a2) = (f a1, f a2)

merge : (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
merge op (x1, y1) (x2, y2) = (op x1 x2, op y1 y2) 

add = merge (+)
subtract = merge (-)
multiply = merge (*)
