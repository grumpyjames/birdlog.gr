module Ui (circle, modal, stopEverything, submitButton) where

import Styles exposing (..)
import Tuple as T

import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as J exposing (Decoder, (:=))
import Result
import Signal exposing (Message)

targetId : Decoder String
targetId = ("target" := ("id" := J.string))        

isTargetId : String -> Decoder Bool
isTargetId id = J.customDecoder targetId (\eyed -> if eyed == id then Result.Ok True else Result.Err "nope!") 

targetWithId : (Bool -> Message) -> String -> String -> Attribute
targetWithId msg event id = on event (isTargetId id) msg

modal : (Signal.Address ()) -> (Int, Int) -> Html -> Html
modal addr size content = 
    let modalId = "modal"
        cancel = targetWithId (\_ -> Signal.message addr ()) "click" modalId
        cell = div (
                    cancel ::
                    (Attr.id modalId) :: 
                    [style [("display", "table-cell"), ("vertical-align", "middle"), ("text-align", "center")]]
                   ) [content]
    in div ([style (absolute ++ dimensions size ++ [("overflow", "hidden"), ("display", "table")])]) [cell]

circle : Int -> (Int, Int) -> Html
circle radius centre = 
    let diameter = 2 * radius
        dims = (diameter, diameter)
        realPosition = centre `T.subtract` (radius, radius)
    in 
      div [style (absolute ++ position realPosition ++ dimensions dims ++ [("border-style", "inset"), ("border-radius", px radius), ("border-color", "indigo"), ("border-width", "thick")])] []

stopEverything = (Options True True)

-- disables autosubmit and associated nonsense
submitButton : Decoder a -> (a -> Message) -> String -> Bool -> Html
submitButton d effect txt disable = 
    button [onWithOptions "click" stopEverything d effect, Attr.disabled disable] [text txt]