module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import MapBox exposing (mapBox)
import Movement exposing (keyState, mouseState)
import Osm exposing (openStreetMap)
import Styles exposing (..)
import Tile exposing (render)
import TouchParser exposing (Gesture(..), gestures)
import Tuple as T
import Types exposing (GeoPoint, Model, TileSource, Zoom)

import Color exposing (rgb)
import Debug exposing (log)
import Graphics.Collage exposing (circle, dotted, collage, outlined, move)
import Graphics.Element exposing  (Element, centered, color, container, flow, layers, middle, right)
import Graphics.Input exposing (customButton, dropDown)
import Html exposing (Attribute, Html, button, div, input, form, text, select, option, fromElement)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick, on, onMouseDown, targetValue)
import Json.Decode as J exposing (Decoder, object2, int, value, (:=))
import List as L
import Maybe as M
import Signal as S
import Text exposing (fromString)
import Window

port hdpi : Bool

defaultTileSrc = mapBoxSource

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = 15.0
        initialModel = Model greenwich initialZoom (False, (0,0)) defaultTileSrc Nothing
    in S.map2 view Window.dimensions (S.foldp applyEvent initialModel events)

clickDecoder : Decoder (Maybe (Int, Int))
clickDecoder = J.map (M.Just) <| object2 (,) ("pageX" := int) ("pageY" := int)

view window model = 
    let mapLayer = render window model        
        styles = style (absolute ++ dimensions window ++ zeroMargin)
        controls = buttons [style absolute] zoomChange.address tileSrc.address
        spottedLayers = M.withDefault [] (M.map (\clicked -> spotLayers clicks.address window clicked) model.clicked)
        clickCatcher = div [styles, (on "dblclick" clickDecoder (S.message clicks.address))] []
    in div [styles] ([mapLayer, clickCatcher, controls] ++ spottedLayers)

vcentred : (Int, Int) -> Html -> Html
vcentred size content = 
    let cell = div [style [("display", "table-cell"), ("vertical-align", "middle")]] [content]
    in div [style (absolute ++ dimensions size ++ [("overflow", "hidden"), ("display", "table")])] [cell]

circleDiv : (Int, Int) -> Html
circleDiv clickPoint = let
    radius = 15
    diameter = 2 * radius
    dims = (diameter, diameter)
    realPosition = clickPoint `T.subtract` (radius, radius)
    in div [style (absolute ++ position realPosition ++ dimensions dims ++ [("border-style", "inset"), ("border-radius", px radius), ("border-color", "indigo"), ("border-width", "thin")])] []

spotLayers : S.Address (Maybe (Int, Int)) -> (Int, Int) -> (Int, Int) -> List Html
spotLayers addr size clickPoint =
    let indicator = circleDiv clickPoint
        saw = text "Spotted: "
        count = input [ Attr.id "count", Attr.type' "number", Attr.placeholder "1" ] []
        bird = input [ Attr.id "species", Attr.type' "text", Attr.placeholder "Puffin" ] []
        at = text " at "
        submit = input [ Attr.type' "submit", onClick addr Nothing ] [text "Save"]
        location = input [ Attr.type' "text", Attr.value (toString clickPoint), Attr.disabled True ] []
        theForm = form [style [("opacity", "0.8")]] [saw, count, bird, at, location, submit]
        content = div [ (style [("text-align", "center")]) ] [theForm]
    in [indicator, vcentred size content]

-- Mailboxes
clicks : S.Mailbox (Maybe (Int, Int))
clicks = S.mailbox Nothing

zoomChange : S.Mailbox ZoomChange
zoomChange = S.mailbox (In 0)

tileSrc : S.Mailbox (Maybe TileSource)
tileSrc = S.mailbox Nothing

-- Events
type ZoomChange = In Float | Out Float

type Events = Z ZoomChange | M (Bool, (Int, Int)) | K (Int, Int) | T (Maybe TileSource) | G (Maybe Gesture) | C (Maybe (Int, Int))

events : Signal Events
events = 
    let zooms = S.map Z <| zoomChange.signal 
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        mouse = S.map M mouseState
        tileSource = S.map T tileSrc.signal
        gests = S.map G gestures 
        klix = S.map C clicks.signal
    in S.mergeMany [tileSource, zooms, gests, klix, mouse, keys]

-- Applying events to the model
applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 M mi -> applyMouse m mi
 G ge -> applyGest m ge
 K ke -> applyKeys m ke 
 C c -> applyClick m c
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

-- Zoom controls and event
newZoom : ZoomChange -> Zoom -> Zoom
newZoom zc z = 
    case zc of
        In a -> z + a
        Out a -> z - a

zoomIn address = ourButton address (In 1) "+"
zoomOut address = ourButton address (Out 1) "-"

applyClick : Model -> Maybe (Int, Int) -> Model
applyClick m c = 
    { m | clicked <- c }

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = { m | zoom <- newZoom zc m.zoom }

-- Events that affect the centre point: drags and arrows
applyMouse : Model -> (Bool, (Int, Int)) -> Model
applyMouse model (isDown, (newX, newY)) = 
    case model.mouseState of
      (False, _) -> { model | mouseState <- (isDown, (newX, newY)) }
      (True, (oldX, oldY)) -> 
          let newModel = applyDrag model (oldX - newX, newY - oldY) 
          in { newModel | mouseState <- (isDown, (newX, newY)) }

applyKeys : Model -> (Int, Int) -> Model
applyKeys = applyDrag

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag = { m | centre <- move m.zoom m.centre drag } 

isInt : Zoom -> Bool
isInt z = (toFloat (round z)) == z

applyGest : Model -> Maybe Gesture -> Model
applyGest m g =
    case g of
      Just ge ->
          case ge of
            Drag (x, y) -> applyDrag m (-1 * x, y)
            otherwise -> m
      Nothing -> m             

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff = 
    let (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ (floor z)))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

accessToken = "pk.eyJ1IjoiZ3J1bXB5amFtZXMiLCJhIjoiNWQzZjdjMDY1YTI2MjExYTQ4ZWU4YjgwZGNmNjUzZmUifQ.BpRWJBEup08Z9DJzstigvg"

mapBoxSource = mapBox hdpi "mapbox.run-bike-hike" accessToken

ons : S.Address (Maybe TileSource) -> Attribute
ons add = let 
    toMsg v = case v of
                "OpenStreetMap" -> Just openStreetMap
                "ArcGIS" -> Just arcGIS
                "MapBox" -> Just mapBoxSource
                otherwise -> Nothing
    in on "change" targetValue (\v -> S.message add (toMsg v))

tileSrcDropDown : S.Address (Maybe TileSource) -> Html
tileSrcDropDown address = 
    let onChange = ons address
    in select [onChange] [option [] [text "MapBox"], option [] [text "OpenStreetMap"], option [] [text "ArcGIS"]]
                

buttons attrs zoomAddress tileSrcAddress = 
    div attrs [zoomIn zoomAddress, zoomOut zoomAddress, tileSrcDropDown tileSrcAddress]

hoverC = rgb 240 240 240
downC = rgb 235 235 235
upC = rgb 248 248 248

ourButton : (S.Address a) -> a -> String -> Html
ourButton address msg txt = 
    let events = [onMouseDown, onClick, (\ad ms -> on "touchend" value (\_ -> Signal.message ad ms))]
    in button (L.map (\e -> e address msg) events) [text txt]