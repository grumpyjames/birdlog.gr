module Buttons (ourButton) where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Signal
import Ui


ourButton : List (String, Bool) -> Signal.Message -> String -> Html.Html
ourButton classes message txt = 
    let sendMsg = (\_ -> message)
        unit = Decode.succeed ()
        attrs = 
            [ Attr.classList classes
            , Events.onWithOptions "click" Ui.stopEverything unit sendMsg
            , Events.onWithOptions "touchend" Ui.stopEverything unit sendMsg
            ]
    in Html.button attrs [Html.text txt]
