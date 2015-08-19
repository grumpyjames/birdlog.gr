module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import CommonLocator exposing (tiley2lat, tilex2long)
import MapBox exposing (mapBox)
import Metacarpal exposing (index, Metacarpal, InnerEvent, Event(..))
import Movement exposing (keyState)
import Osm exposing (openStreetMap)
import Styles exposing (..)
import Tile exposing (render)
import Tuple as T
import Types exposing (GeoPoint, Model, TileSource, Zoom, Sighting)
import Ui

import Color exposing (rgb)
import Debug exposing (crash, log)
import Http
import Html exposing (Attribute, Html, button, div, input, form, text, select, option, fromElement)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (Options, onClick, on, onWithOptions, onMouseDown, targetValue)
import Json.Decode as J exposing (Decoder, object2, int, value, (:=), fail)
import List as L
import Maybe as M
import Result exposing (Result(..))
import Signal as S
import String
import Task exposing (Task)
import Time exposing (Time)
import Window

port hdpi : Bool

defaultTileSrc = mapBoxSource

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = 15.0
        initialModel = Model hdpi greenwich initialZoom (False, (0,0)) defaultTileSrc (0.0, Nothing) (Sighting 1 "pheasant" greenwich 0.0) False
    in S.map2 view Window.dimensions (S.foldp applyEvent initialModel events)

clickDecoder : Decoder (Maybe (Int, Int))
clickDecoder = J.map (M.Just) <| object2 (,) ("pageX" := int) ("pageY" := int)

view window model = 
    let mapLayer = render window model        
        styles = style (absolute ++ dimensions window ++ zeroMargin)
        controls = buttons [style absolute] actions.address
        toSpotLayer clicked = spotLayers actions.address window model clicked
        spottedLayers = M.withDefault [] (M.map toSpotLayer (snd model.clicked))
        dblClick = index.attr metacarpal.address
        clickCatcher = div (dblClick ++ [styles]) []
    in div [styles] ([mapLayer, clickCatcher, controls] ++ spottedLayers)

targetId : Decoder String
targetId = ("target" := ("id" := J.string))        

isTargetId : String -> Decoder Bool
isTargetId id = J.customDecoder targetId (\eyed -> if eyed == id then Result.Ok True else Result.Err "nope!") 

targetWithId : (Bool -> S.Message) -> String -> String -> Attribute
targetWithId msg event id = on event (isTargetId id) msg

unsafeToInt : String -> Int
unsafeToInt s = 
    let res = String.toInt s
    in case res of
         Ok i -> i
         Err e -> crash "whoops"

spotLayers : S.Address (Events) -> (Int, Int) -> Model -> (Int, Int) -> List Html
spotLayers addr win model clickPoint =
    let indicator = Ui.circle (20 * (if model.hdpi then 2 else 1)) clickPoint
        clickLoc = toGeopoint win model clickPoint 
        nada = (\_ -> S.message addr (C Nothing))
        modalId = "modal"
        cancel = targetWithId nada "click" modalId
        saw = text "Spotted: "
        countDecoder = J.map (Count << unsafeToInt) targetValue
        sendFormChange fc = S.message addr (F fc)
        count = input [ Attr.id "count", Attr.type' "number", Attr.placeholder "1",  on "change" countDecoder sendFormChange, on "input" countDecoder sendFormChange] []
        speciesDecoder = J.map Species targetValue
        bird = input [ Attr.id "species", Attr.type' "text", Attr.placeholder "Puffin", on "input" speciesDecoder sendFormChange] []
        at = text " at "
        ins = model.sighting
        sighting = { ins | location <- clickLoc, time <- fst model.clicked }
        submit = Ui.submitButton (J.succeed (Just sighting)) (\s -> S.message addr (S s)) "Save" model.progress
        theForm = form [style [("opacity", "0.8")]] [saw, count, bird, submit]
    in [indicator, Ui.modal [Attr.id modalId, cancel] win theForm]

-- Mailboxes
actions : S.Mailbox Events
actions = S.mailbox N

metacarpal : S.Mailbox InnerEvent
metacarpal = S.mailbox index.zero

type FormChange = Species String
                | Count Int

onSuccess : (Maybe String) -> Task x ()
onSuccess res = S.send actions.address (H res)

postSighting : Sighting -> Task Http.Error ()
postSighting s = 
    let body = Http.string <| toString s
        request = Http.post (J.succeed (Just "woot!")) "/sightings" body 
    in Task.mapError (Debug.log "ohshit") (request `Task.andThen` onSuccess)

pickSightings : Events -> Maybe (Maybe Sighting)
pickSightings action = 
    case action of
      S s -> Just s
      otherwise -> Nothing

sig : Maybe Sighting -> Task Http.Error ()
sig m = 
    case m of 
      Nothing -> Task.succeed ()
      Just s -> postSighting s

port sightingSubmissions : Signal (Task Http.Error ())
port sightingSubmissions = 
    S.map sig <| S.filterMap pickSightings Nothing actions.signal

-- Events
type ZoomChange = In Float | Out Float

type Events = Z ZoomChange | K (Int, Int) | T (Maybe TileSource) | C (Maybe (Int, Int)) | O (Maybe Event) | F FormChange | S (Maybe Sighting) | H (Maybe String) | N

events : Signal (Time, Events)
events = 
    let keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        ot = S.map O <| index.sign metacarpal.signal
    in Time.timestamp <| S.mergeMany [actions.signal, keys, ot]

-- Applying events to the model
applyEvent : (Time, Events) -> Model -> Model
applyEvent (t, e) m = case e of
 Z zo -> applyZoom m zo
 K ke -> applyKeys m ke 
 C c -> applyClick m t c
 O o -> applyO m t o
 F fc -> applyFc m fc
 S s -> applyS m s
 H h -> applyH m h
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

applyH : Model -> Maybe String -> Model
applyH m s = 
    case s of 
      Just woo -> { m | progress <- False, clicked <- (0.0, Nothing) }
      Nothing -> m

applyS : Model -> Maybe Sighting -> Model
applyS m s = 
    case s of
      Just sght -> { m | progress <- True }
      Nothing -> m

applyFc : Model -> FormChange -> Model
applyFc m fc = 
    let updateCount sg c = { sg | count <- c }
        updateSpecies sg s = { sg | name <- s }
    in 
      case fc of
        Count c -> { m | sighting <- (updateCount m.sighting c)}
        Species s -> { m | sighting <- (updateSpecies m.sighting s)}

applyO : Model -> Time -> Maybe Event -> Model
applyO m t o = 
    case o of
      Just e -> 
          case e of
            Metacarpal.Drag pn ->
                applyDrag m ((1, -1) `T.multiply` pn)
            DoubleClick pn ->
                applyClick m t (Just pn)
            LongPress pn->
                applyClick m t (Just pn)
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

zoomIn address = ourButton address (Z (In 1)) "+"
zoomOut address = ourButton address (Z (Out 1)) "-"

applyClick : Model -> Time -> Maybe (Int, Int) -> Model
applyClick m t c = 
    { m | clicked <- (t, c) }

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

ons : S.Address (Events) -> Attribute
ons add = let 
    toMsg v = case v of
                "OpenStreetMap" -> Just openStreetMap
                "ArcGIS" -> Just arcGIS
                "MapBox" -> Just mapBoxSource
                otherwise -> Nothing
    in on "change" targetValue (\v -> S.message add (T (toMsg v)))

tileSrcDropDown : S.Address (Events) -> Html
tileSrcDropDown address = 
    let onChange = ons address
    in select [onChange] [option [] [text "MapBox"], option [] [text "OpenStreetMap"], option [] [text "ArcGIS"]]
                

buttons attrs actionAddress = 
    div attrs [zoomIn actionAddress, zoomOut actionAddress, tileSrcDropDown actionAddress]

hoverC = rgb 240 240 240
downC = rgb 235 235 235
upC = rgb 248 248 248

ourButton : (S.Address a) -> a -> String -> Html
ourButton address msg txt = 
    let events = [onMouseDown, onClick, (\ad ms -> on "touchend" value (\_ -> Signal.message ad ms))]
    in button (L.map (\e -> e address msg) events) [text txt]

-- lon min: -180
-- lat min : 85.0511

toGeopoint : (Int, Int) -> Model -> (Int, Int) -> GeoPoint
toGeopoint win model clk = 
    let centre = model.centre
        tileSize = model.tileSource.tileSize
        zoom = model.zoom
        middle = T.map (\a -> a // 2) win
        relative = T.map (\p -> (toFloat p) / (toFloat tileSize)) (clk `T.subtract` middle)
        offset = GeoPoint (tiley2lat (snd relative) zoom) (tilex2long (fst relative) zoom)
    in GeoPoint (-85.0511 + centre.lat + (tiley2lat (snd relative) zoom)) (180.0 + centre.lon + (tilex2long (fst relative) zoom))