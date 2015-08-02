module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import MapBox exposing (mapBox)
import Metacarpal exposing (index, Metacarpal, InnerEvent, Event(..))
import Movement exposing (keyState)
import Osm exposing (openStreetMap)
import Styles exposing (..)
import Tile exposing (render)
import Tuple as T
import Types exposing (GeoPoint, Model, TileSource, Zoom)

import Color exposing (rgb)
import Debug exposing (crash, log)
import Graphics.Collage exposing (circle, dotted, collage, outlined, move)
import Graphics.Element exposing  (Element, centered, color, container, flow, layers, middle, right)
import Graphics.Input exposing (customButton, dropDown)
import Html exposing (Attribute, Html, button, div, input, form, text, select, option, fromElement)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (Options, onClick, on, onWithOptions, onMouseDown, targetValue)
import Json.Decode as J exposing (Decoder, object2, int, value, (:=), fail)
import List as L
import Maybe as M
import Result
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
        dblClick = index.attr metacarpal.address
        clickCatcher = div (dblClick ++ [styles]) []
    in div [styles] ([mapLayer, clickCatcher, controls] ++ spottedLayers)

vcentred : String -> List Attribute -> (Int, Int) -> Html -> Html
vcentred ident attrs size content = 
    let cell = div (attrs ++ [style [("display", "table-cell"), ("vertical-align", "middle"), ("text-align", "center")], Attr.id ident]) [content]
    in div ([style (absolute ++ dimensions size ++ [("overflow", "hidden"), ("display", "table")])]) [cell]

circleDiv : (Int, Int) -> Html
circleDiv clickPoint = let
    radius = 15
    diameter = 2 * radius
    dims = (diameter, diameter)
    realPosition = clickPoint `T.subtract` (radius, radius)
    in div [style (absolute ++ position realPosition ++ dimensions dims ++ [("border-style", "inset"), ("border-radius", px radius), ("border-color", "indigo"), ("border-width", "thin")])] []

dead : S.Mailbox (String)
dead = S.mailbox ""

-- can't stop the prop
stopTheProp : String -> Attribute
stopTheProp event = 
    let opts = Options True False
    in onWithOptions event opts (J.succeed "!") (Signal.message dead.address)

targetId : Decoder String
targetId = ("target" := ("id" := J.string))        

isTargetId : String -> Decoder Bool
isTargetId id = J.customDecoder targetId (\eyed -> if eyed == id then Result.Ok True else Result.Err "nope!") 

targetWithId : (Bool -> S.Message) -> String -> String -> Attribute
targetWithId msg event id = on event (isTargetId id) msg

spotLayers : S.Address (Maybe (Int, Int)) -> (Int, Int) -> (Int, Int) -> List Html
spotLayers addr size clickPoint =
    let indicator = circleDiv clickPoint
        cancel = targetWithId  (\_ -> S.message addr Nothing) "click" "modal"
        saw = text "Spotted: "
        count = input [ Attr.id "count", Attr.type' "number", Attr.placeholder "1" ] []
        bird = input [ Attr.id "species", Attr.type' "text", Attr.placeholder "Puffin" ] []
        at = text " at "
        submit = input [ Attr.type' "submit", cancel, stopTheProp "click"] [text "Save"]
        location = input [ Attr.type' "text", Attr.value (toString clickPoint), Attr.disabled True ] []
        theForm = form [style [("opacity", "0.8")]] [saw, count, bird, at, location, submit]
    in [indicator, vcentred "modal" [cancel] size theForm]

-- Mailboxes
clicks : S.Mailbox (Maybe (Int, Int))
clicks = S.mailbox Nothing

metacarpal : S.Mailbox InnerEvent
metacarpal = S.mailbox index.zero 

zoomChange : S.Mailbox ZoomChange
zoomChange = S.mailbox (In 0)

tileSrc : S.Mailbox (Maybe TileSource)
tileSrc = S.mailbox Nothing

-- Events
type ZoomChange = In Float | Out Float

type Events = Z ZoomChange | K (Int, Int) | T (Maybe TileSource) | C (Maybe (Int, Int)) | O (Maybe Event)

events : Signal Events
events = 
    let zooms = S.map Z <| zoomChange.signal 
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        tileSource = S.map T tileSrc.signal
        klix = S.map C clicks.signal
        ot = S.map O <| index.sign metacarpal.signal
    in S.mergeMany [tileSource, zooms, klix, keys, ot]

-- Applying events to the model
applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 K ke -> applyKeys m ke 
 C c -> applyClick m c
 O o -> applyO m o
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

applyO : Model -> Maybe Event -> Model
applyO m o = 
    case o of
      Just e -> 
          case e of
            Metacarpal.Drag pn ->
                applyDrag m ((1, -1) `T.multiply` pn)
            DoubleClick pn ->
                applyClick m (Just pn)
            LongPress pn->
                applyClick m (Just pn)
            otherwise ->
                m
      otherwise -> 
          m

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

applyKeys : Model -> (Int, Int) -> Model
applyKeys = applyDrag

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag = { m | centre <- move m.zoom m.centre drag } 

isInt : Zoom -> Bool
isInt z = (toFloat (round z)) == z

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