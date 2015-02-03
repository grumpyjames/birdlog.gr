module Tuple (zeroT, mergeT, mapT, addT, multiplyT, subtractT) where

zeroT = (0, 0)

mapT : (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

mergeT : (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
mergeT op (x1, y1) (x2, y2) = (op x1 x2, op y1 y2) 

addT = mergeT (+)
subtractT = mergeT (-)
multiplyT = mergeT (*)
