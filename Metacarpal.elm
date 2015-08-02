module Metacarpal (Event(..), InnerEvent, Metacarpal, index) where

import Tuple as T

import Debug exposing (log)
import Html exposing (Attribute)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as J exposing (Decoder, object2, float, int, succeed, value, (:=))
import List exposing (filter, filterMap, head, length)
import Maybe as M
import Signal as S

type Event
    = Drag (Int, Int)
    | DoubleClick (Int, Int)
    | LongPress (Int, Int)

type alias Touch = 
    { 
      position: (Int, Int)
    , id : Int
    }

type InnerEvent
    = MouseDown (Int, Int)
    | MouseMove (Int, Int)
    | MouseOut (Int, Int)
    | MouseUp (Int, Int)
    | DblClick (Int, Int)
    | TouchStart (List Touch)
    | TouchMove (List Touch)
    | TouchEnd

type State
    = InDrag (Int, Int)
    | InGesture Bool (List Touch)
    | Clean

type alias Metacarpal i e =
    {
      sign : Signal i -> Signal (Maybe e)
    , attr : (S.Address i) -> List Attribute
    , zero : i
    }

index : Metacarpal InnerEvent Event
index = Metacarpal sgn interactions (MouseOut (0, 0))

sgn : Signal InnerEvent -> Signal (Maybe Event)
sgn sie = S.map snd <| S.foldp parse (Clean, M.Nothing) sie

parse : InnerEvent -> (State, Maybe Event) -> (State, Maybe Event)
parse ie s = 
    case (ie, fst s) of
      (DblClick posn, _) 
          -> (Clean, M.Just (DoubleClick posn))
      (MouseDown posn, _)
          -> (InDrag posn, Nothing)
      (MouseMove posn, InDrag oldPosn)
          -> (InDrag posn, M.Just (Drag (oldPosn `T.subtract` posn)))
      (MouseMove posn, z)
          -> (z, Nothing)
      (MouseOut posn, InDrag oldPosn)
          -> (Clean, M.Just (Drag (oldPosn `T.subtract` posn)))
      (MouseOut posn, _)
          -> (Clean, M.Nothing)
      (TouchStart ts, _)
          -> (InGesture False ts, M.Nothing)
      (TouchMove newTs, InGesture _ oldTs)
          -> (InGesture True newTs, parseTouchEvent newTs oldTs)
      (TouchMove newTs, _)
          -> (Clean, M.Nothing)
      (TouchEnd, InGesture moved oldTouches)
          -> (Clean, if moved then M.Nothing else M.map (\t -> LongPress t.position) (head oldTouches))
      (TouchEnd, _)
          -> (Clean, M.Nothing)
      otherwise 
          -> (Clean, M.Nothing)

parseTouchEvent : List Touch -> List Touch -> Maybe Event
parseTouchEvent newTouches oldTouches = 
    if ((length oldTouches == 1) && (length newTouches == 1)) then parseEvent newTouches oldTouches else Nothing

parseEvent : List Touch -> List Touch -> Maybe Event
parseEvent oldTs newTs =
    let matches = pairBy (\t -> t.id) oldTs newTs
    in parseOne matches

parseOne : List (Touch, Touch) -> Maybe Event
parseOne ts =
    case ts of
      (p1 :: []) -> Just <| parseDrag p1
      otherwise -> Nothing

parseDrag : (Touch, Touch) -> Event
parseDrag (t1, t2) = Drag (t2.position `T.subtract` t1.position)

-- by this point we know length of each list is small, so go O(n)
-- _never_ export this function!
pairBy : (a -> b) -> List a -> List a -> List (a, a)
pairBy f xs ys =
    let pairOf x = find f (f x) ys
        maybeMatch x = Maybe.map (\y -> (x, y)) <| pairOf x 
    in filterMap maybeMatch xs

find : (a -> b) -> b -> List a -> Maybe a
find feature needle = head << filter (\x -> (feature x) == needle)

prev = Options True True

interactions : S.Address (InnerEvent) -> List Attribute
interactions addr = 
    [
      on "mousedown" positionDecoder (\posn -> S.message addr (MouseDown posn))
    , on "mouseup" positionDecoder (\posn -> S.message addr (MouseOut posn))
    , on "mouseout" positionDecoder (\posn -> S.message addr (MouseOut posn))
    , on "mousemove" positionDecoder (\posn -> S.message addr (MouseMove posn))
    , onWithOptions "dblclick" prev positionDecoder (\posn -> S.message addr (DblClick posn))
    , onWithOptions "touchstart" prev touchDecoder (\ts -> S.message addr (TouchStart ts))
    , onWithOptions "touchmove" prev touchDecoder (\ts -> S.message addr (TouchMove ts))
    , onWithOptions "touchend" prev (succeed 1) (\t -> S.message addr (TouchEnd))
    , onWithOptions "touchleave" prev (succeed 1) (\t -> S.message addr (TouchEnd))
    ]

-- decoders
positionDecoder : Decoder (Int, Int)
positionDecoder = object2 (,) ("pageX" := int) ("pageY" := int)

floatyPnDecoder : Decoder (Int, Int)
floatyPnDecoder = J.map (T.map floor) <| object2 (,) ("pageX" := float) ("pageY" := float)

touchDecoder : Decoder (List Touch)
touchDecoder = 
    let         
      oneTouch = J.map (\t -> [t]) <| object2 Touch floatyPnDecoder ("identifier" := int)
    in 
      ("touches" := ("0" := oneTouch))