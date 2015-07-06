module TouchParser (main, Gesture(..), gestures) where

import Graphics.Element exposing (Element, show)
import List as L exposing (filter, filterMap, head, length, map, (::))
import Maybe as M exposing (map)
import Signal as S exposing (constant, foldp, map)
import Touch exposing (Touch, touches)
import Tuple as T

main : Signal Element
main = S.map show <| foldp (::) [] <| foldp parse (TouchState [] Nothing) <| S.map (L.map clone) touches 

gestures : Signal (Maybe Gesture)
gestures = S.map (\t -> t.maybeGesture) <| foldp parse (TouchState [] Nothing) <| S.map (L.map clone) touches 

type alias TouchState = {
      oldTouches : List Touch,
      maybeGesture : Maybe Gesture
}

clone : Touch -> Touch
clone t = Touch (t.x * 1) (t.y * 1) (t.id * 1) 0 0 0

t : (Int, Int) -> Int -> Touch
t (x, y) id = Touch x y id 0 0 0

parse : List Touch -> TouchState -> TouchState
parse newTs oldState = case oldState.oldTouches of
  [] -> TouchState (L.map clone newTs) (Just Start)
  -- just one touch for now
  (x1 :: x2 :: xs) -> oldState
  otherwise -> TouchState (L.map clone newTs) (maybeParse oldState.oldTouches newTs)  

type Gesture = Start | Drag (Int, Int) | End

maybeParse : List Touch -> List Touch -> Maybe Gesture
maybeParse oldTs newTs =
    let oldLen = (length oldTs)
        newLen = (length newTs)
    in if (oldLen > newLen) then Just End
       else if (newLen > oldLen) then Just Start
       else parseEvent oldTs newTs

pt : Touch -> (Int, Int)
pt t = (t.x * 1, t.y * 1)

parseEvent : List Touch -> List Touch -> Maybe Gesture
parseEvent oldTs newTs =
    let matches = pairBy (\t -> t.id) oldTs newTs
    in parseOne matches

parseOne : List (Touch, Touch) -> Maybe Gesture
parseOne ts =
    case ts of
      (p1 :: []) -> Just <| parseDrag p1
      otherwise -> Nothing

point : Touch -> (Float, Float)
point t = T.map toFloat (t.x, t.y)

size : (Float, Float) -> Float
size (x, y) = sqrt (x^2 + y^2)


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