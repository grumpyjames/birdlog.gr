module Ui (circle, modal, submitButton) where

import Styles exposing (..)
import Tuple as T

import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions, Options)
import Json.Decode exposing (Decoder)
import Signal exposing (Message)

modal : List Attribute -> (Int, Int) -> Html -> Html
modal attrs size content = 
    let cell = div (attrs ++ [style [("display", "table-cell"), ("vertical-align", "middle"), ("text-align", "center")]]) [content]
    in div ([style (absolute ++ dimensions size ++ [("overflow", "hidden"), ("display", "table")])]) [cell]

circle : (Int, Int) -> Html
circle centre = 
    let radius = 15
        diameter = 2 * radius
        dims = (diameter, diameter)
        realPosition = centre `T.subtract` (radius, radius)
    in 
      div [style (absolute ++ position realPosition ++ dimensions dims ++ [("border-style", "inset"), ("border-radius", px radius), ("border-color", "indigo"), ("border-width", "thin")])] []

-- disables autosubmit and associated nonsense
submitButton : Decoder a -> (a -> Message) -> String -> Bool -> Html
submitButton d effect txt disable = 
    let disableThings = (Options True True)
    in button [onWithOptions "click" disableThings d effect, Html.Attributes.disabled disable] [text txt]