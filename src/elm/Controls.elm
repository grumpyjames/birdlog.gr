module Controls (controls, locationButton, zoomIn, zoomOut) where

import Buttons exposing (ourButton)
import Model exposing (Events(..), Model)
import Types exposing (TileSource)

import Dict as D exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Events
import Json.Decode as JD
import List as L
import Maybe as M
import Signal as S

controls : Model -> Dict String TileSource -> List Attribute -> S.Address Events -> S.Address () -> Html
controls model sources attrs actionAddress locationRequestAddress = 
    Html.div attrs [ zoomIn actionAddress
                   , zoomOut actionAddress
                   , locationButton model.locationProgress locationRequestAddress
                   , tileSrcDropDown sources actionAddress]

cont : Dict String TileSource -> String -> Result String Events
cont srcs v = D.get v srcs |>
              M.map (\a -> Result.Ok (TileSourceChange a)) |> 
              M.withDefault (Result.Err ("source " ++ (toString v) ++ " not found"))

ons : Dict String TileSource -> S.Address (Events) -> List Attribute
ons srcs addr = 
    let decoder = JD.customDecoder Html.Events.targetValue (cont srcs)
    in [Html.Events.on "change" decoder (S.message addr)]

tileSrcDropDown : Dict String TileSource -> S.Address (Events) -> Html
tileSrcDropDown srcs address = 
    Html.select (ons srcs address) 
            <| L.map (\srcName -> Html.option [] [Html.text (toString srcName)]) 
            <| D.keys srcs

zoomIn address = ourButton [("circ", True), ("zoom", True)] (S.message address (ZoomChange 1)) "+"
zoomOut address = ourButton [("circ", True), ("zoom", True)] (S.message address (ZoomChange (-1))) "-"
locationButton inProgress address = ourButton [("circ", True), ("location", True), ("inprogress", inProgress)] (S.message address ()) ""
