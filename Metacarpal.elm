module Metacarpal (Event(..), InnerEvent, Metacarpal, index) where

import Tuple as T

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as J exposing (Decoder, object2, int, value, (:=))
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
    | Touches (List Touch)

type State
    = InDrag (Int, Int)
    | InGesture (List Touch)
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
          -> (InDrag posn, M.Just (Drag ((1, -1) `T.multiply` (oldPosn `T.subtract` posn))))
      (MouseMove posn, z)
          -> (z, Nothing)
      (MouseOut posn, InDrag oldPosn)
          -> (Clean, M.Just (Drag ((1, -1) `T.multiply` (oldPosn `T.subtract` posn))))
      (MouseOut posn, _)
          -> (Clean, M.Nothing)
      (Touches ts, InGesture oldTouches)
          -> (InGesture ts, parseTouchEvent ts oldTouches)
      (Touches ts, _)
          -> (InGesture ts, M.Nothing)
      otherwise 
          -> (Clean, M.Nothing)

parseTouchEvent : List Touch -> List Touch -> Maybe Event
parseTouchEvent newTouches oldTouches = M.Nothing

interactions : S.Address (InnerEvent) -> List Attribute
interactions addr = 
    [
      on "mousedown" positionDecoder (\posn -> S.message addr (MouseDown posn))
    , on "mouseup" positionDecoder (\posn -> S.message addr (MouseOut posn))
    , on "mouseout" positionDecoder (\posn -> S.message addr (MouseOut posn))
    , on "mousemove" positionDecoder (\posn -> S.message addr (MouseMove posn))
    , on "dblclick" positionDecoder (\posn -> S.message addr (DblClick posn))
    , on "touchstart" touchDecoder (\ts -> S.message addr (Touches ts)) 
    ]

-- decoders
positionDecoder : Decoder (Int, Int)
positionDecoder = object2 (,) ("pageX" := int) ("pageY" := int)

touchDecoder : Decoder (List Touch)
touchDecoder =
    let         
      oneTouch = object2 Touch positionDecoder ("identifier" := int)
    in 
      ("touches" := J.list oneTouch)