module TouchParser (main, Gesture(..), gestures) where

import Graphics.Element exposing (Element, show)
import List as L exposing (filter, filterMap, head, length, map)
import Maybe as M exposing (map)
import Signal as S exposing (constant, foldp, map)
import Touch exposing (Touch, touches)
import Tuple as T

main : Signal Element
main = S.map show <| foldp parse (TouchState [] Nothing) <| S.map (L.map clone) touches 

-- S.constant <| show <| parse [(t (5, 11) 0)] (TouchState [(t (2, 3) 0)] Nothing) 

-- S.map show <| foldp parse (TouchState [] Nothing) touches

gestures : Signal (Maybe Gesture)
gestures = S.map (\t -> t.maybeGesture) <| foldp parse (TouchState [] Nothing) <| S.map (L.map clone) touches 


test : Signal Element
test = 
    let f = maybeParse
    in constant <| show <|
           [
            f [(t (0, 1) 0), (t (0, 0) 1)] [(t (0, 1) 0), (t (0, 0) 1)],
            f [(t (0, 0) 0), (t (0, 1) 1)] [(t (0, 0) 0), (t (1, 0) 1)],
            f [(t (0, 0) 0), (t (0, 1) 1)] [(t (0, 0) 0), (t (0, 2) 1)],
            f [(t (0, 0) 0)] [(t (0, 3) 0)],
            f [(t (0, 0) 0)] [(t (0, 3) 0), (t (1, 1) 1)]
           ]

type alias TouchState = {
      oldTouches : List Touch,
      maybeGesture : Maybe Gesture
}

clone : Touch -> Touch
clone t = Touch (t.x * 1) (t.y * 1) (t.id * 1) 0 0 0

t : (Int, Int) -> Int -> Touch
t (x, y) id = Touch x y id 0 0 0
-- main = S.map show <| foldp parse (TouchState [] Nothing) touches

parse : List Touch -> TouchState -> TouchState
parse newTs oldState = case oldState.oldTouches of
  [] -> TouchState (L.map clone newTs) Nothing
-- don't even attempt to parse more than three touches
  (x1 :: x2 :: x3 :: x4 :: xs) -> oldState
  otherwise -> TouchState (L.map clone newTs) (parseEvent oldState.oldTouches newTs)  

type alias AffineComponents = {
      rotation : Float,
      scale : (Float, Float)
}

type Gesture = Affine AffineComponents | Drag (Int, Int)

maybeParse : List Touch -> List Touch -> Maybe Gesture
maybeParse oldTs newTs = if (length oldTs) == (length newTs) then parseEvent oldTs newTs else Nothing 

pt : Touch -> (Int, Int)
pt t = (t.x * 1, t.y * 1)

parseEvent : List Touch -> List Touch -> Maybe Gesture
parseEvent oldTs newTs =
    let matches = pairBy (\t -> t.id) oldTs newTs
    in parseOne matches

parseOne : List (Touch, Touch) -> Maybe Gesture
parseOne ts =
    case ts of
      -- The ideal case; can precisely determine a transform if we wish
      -- We choose not to for now!
      (p1 :: p2 :: p3 :: []) -> Nothing
      -- Assumption: deltas are small.
      (p1 :: p2 :: []) -> Just <| fromTwoTouchPairs p1 p2
      -- This is safe.
      (p1 :: []) -> Just <| parseDrag p1
      otherwise -> Nothing

point : Touch -> (Float, Float)
point t = T.map toFloat (t.x, t.y)

size : (Float, Float) -> Float
size (x, y) = sqrt (x^2 + y^2)

fromTwoTouchPairs : (Touch, Touch) -> (Touch, Touch) -> Gesture
fromTwoTouchPairs (x1, y1) (x2, y2) =
    let a = (point x2) `T.subtract` (point x1)
        b = (point y2) `T.subtract` (point y1)
        (dotx, doty) = (a `T.multiply` b)
        theta = acos ((dotx + doty) / ((size a) * (size b)))
        nanToZero f = if isNaN f then 0 else f
        scale = T.map (abs << nanToZero) (b `T.divide` a)
    in Affine <| AffineComponents theta scale

parseDrag : (Touch, Touch) -> Gesture
parseDrag (t1, t2) = Drag (t2.x - t1.x, t2.y - t1.y)

-- by this point we know length of each list is small, so go O(n)
-- _never_ export this function!
pairBy : (a -> b) -> List a -> List a -> List (a, a)
pairBy f xs ys =
    let pairOf x = find f (f x) ys
        maybeMatch x = Maybe.map (\y -> (x, y)) <| pairOf x 
    in filterMap maybeMatch xs

find : (a -> b) -> b -> List a -> Maybe a
find feature needle = head << filter (\x -> (feature x) == needle) 