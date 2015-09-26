module Metacarpal ( Event(..)
                  , InnerEvent
                  , Metacarpal
                  , index) where

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
    | InGesture (List Touch) Int
    | Clean

type alias Metacarpal =
    {
      signal : Signal (Maybe Event)
    , attr : List Attribute
    }

metacarpal : S.Mailbox InnerEvent
metacarpal = S.mailbox (MouseOut (0, 0))

index : Metacarpal
index = Metacarpal sgn interactions

sgn : Signal (Maybe Event)
sgn = S.map snd <| S.foldp parse (Clean, M.Nothing) metacarpal.signal

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
          -> (InGesture ts 0, M.Nothing)
      (TouchMove newTs, InGesture oldTs moved)
          -> parseTouchEvent moved newTs oldTs
      (TouchMove newTs, _)
          -> (Clean, M.Nothing)
      (TouchEnd, InGesture oldTouches moved)
          -> (Clean, if moved > 25 then M.Nothing else M.map (\t -> LongPress t.position) (head oldTouches))
      (TouchEnd, _)
          -> (Clean, M.Nothing)
      otherwise 
          -> (Clean, M.Nothing)

parseTouchEvent : Int -> List Touch -> List Touch -> (State, Maybe Event)
parseTouchEvent moved newTouches oldTouches = 
    if ((length oldTouches == 1) && (length newTouches == 1)) 
    then parseEvent moved newTouches oldTouches
    else (InGesture newTouches moved, Nothing)

parseEvent : Int -> List Touch -> List Touch -> (State, Maybe Event)
parseEvent moved oldTs newTs =
    let matches = pairBy (\t -> t.id) oldTs newTs
    in parseOne (InGesture oldTs) moved matches

parseOne : (Int -> State) -> Int -> List (Touch, Touch) -> (State, Maybe Event)
parseOne f moved ts =
    case ts of
      (p :: []) -> 
          let dragEvent = ((snd p).position `T.subtract` (fst p).position)
          in (f ((size dragEvent) + moved), Just <| Drag dragEvent)
      otherwise -> (f moved, Nothing)

size : (Int, Int) -> Int
size (a, b) = (abs a) + (abs b)

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

interactions : List Attribute
interactions =
    let addr = metacarpal.address
    in
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