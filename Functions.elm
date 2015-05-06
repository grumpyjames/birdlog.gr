module Functions (andThen) where

andThen : (a -> b) -> (a -> b -> c) -> a -> c
andThen f g = \a -> g a (f a)
