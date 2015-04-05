module Functions (chain) where

chain : (a -> b) -> (a -> b -> c) -> a -> c
chain f g = \a -> g a (f a)
