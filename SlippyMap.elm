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

import Color exposing (rgb)
import Debug exposing (crash, log)
import Graphics.Collage exposing (circle, dotted, collage, outlined, move)
import Graphics.Element exposing  (Element, centered, color, container, flow, layers, middle, right)
import Graphics.Input exposing (customButton, dropDown)
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
import Text exposing (fromString)
import Window

port hdpi : Bool

defaultTileSrc = mapBoxSource

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = 15.0
        initialModel = Model greenwich initialZoom (False, (0,0)) defaultTileSrc Nothing (Sighting 1 "pheasant" greenwich)
    in S.map2 view Window.dimensions (S.foldp applyEvent initialModel events)

clickDecoder : Decoder (Maybe (Int, Int))
clickDecoder = J.map (M.Just) <| object2 (,) ("pageX" := int) ("pageY" := int)

view window model = 
    let mapLayer = render window model        
        styles = style (absolute ++ dimensions window ++ zeroMargin)
        controls = buttons [style absolute] zoomChange.address tileSrc.address
        toSpotLayer clicked = spotLayers formChange.address clicks.address sightings.address window model clicked
        spottedLayers = M.withDefault [] (M.map toSpotLayer model.clicked)
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

spotLayers : S.Address (FormChange) -> S.Address (Maybe (Int, Int)) -> S.Address (Maybe (Sighting)) -> (Int, Int) -> Model -> (Int, Int) -> List Html
spotLayers fc addr sight win model clickPoint =
    let indicator = circleDiv clickPoint
        clickLoc = toGeopoint win model clickPoint 
        nada = (\_ -> S.message addr Nothing)
        cancel = targetWithId nada "click" "modal"
        saw = text "Spotted: "
        countDecoder = J.map (Count << unsafeToInt) targetValue
        count = input [ Attr.id "count", Attr.type' "number", Attr.placeholder "1",  on "change" countDecoder (S.message fc), on "input" countDecoder (S.message fc)] []
        speciesDecoder = J.map Species targetValue
        bird = input [ Attr.id "species", Attr.type' "text", Attr.placeholder "Puffin", on "input" speciesDecoder (S.message fc)] []
        at = text " at "
        submit = button [onWithOptions "click" (Options True True) (J.succeed (Just model.sighting)) (S.message sight)] [text "Save"]
        location = input [ Attr.type' "text", Attr.value (toString clickLoc), Attr.disabled True ] []
        theForm = form [style [("opacity", "0.8")]] [saw, count, bird, at, location, submit]
    in [indicator, vcentred "modal" [cancel] win theForm]

-- Mailboxes
clicks : S.Mailbox (Maybe (Int, Int))
clicks = S.mailbox Nothing

metacarpal : S.Mailbox InnerEvent
metacarpal = S.mailbox index.zero

type FormChange = Species String
                | Count Int

formChange : S.Mailbox FormChange
formChange = S.mailbox (Species "")

zoomChange : S.Mailbox ZoomChange
zoomChange = S.mailbox (In 0)

httpSuccess : S.Mailbox String
httpSuccess = S.mailbox ""

onSuccess : String -> Task x ()
onSuccess res = S.send httpSuccess.address res

postSighting : Sighting -> Task Http.Error ()
postSighting s = 
    let body = Http.string <| toString s
        request = Http.post (J.succeed "woot!") "/sightings" body 
    in request `Task.andThen` onSuccess

sightings : S.Mailbox (Maybe Sighting)
sightings = S.mailbox Nothing

port sightingSubmissions : Signal (Task Http.Error ())
port sightingSubmissions = 
    let sig m = 
            case m of 
              Nothing -> Task.succeed ()
              Just s -> postSighting s
    in S.map sig sightings.signal

tileSrc : S.Mailbox (Maybe TileSource)
tileSrc = S.mailbox Nothing

-- Events
type ZoomChange = In Float | Out Float

type Events = Z ZoomChange | K (Int, Int) | T (Maybe TileSource) | C (Maybe (Int, Int)) | O (Maybe Event) | F FormChange

events : Signal Events
events = 
    let zooms = S.map Z <| zoomChange.signal 
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        tileSource = S.map T tileSrc.signal
        klix = S.map C clicks.signal
        ot = S.map O <| index.sign metacarpal.signal
        fc = S.map F formChange.signal
    in S.mergeMany [fc, tileSource, zooms, klix, keys, ot]

-- Applying events to the model
applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 K ke -> applyKeys m ke 
 C c -> applyClick m c
 O o -> applyO m o
 F fc -> applyFc m fc
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

applyFc : Model -> FormChange -> Model
applyFc m fc = 
    let updateCount sg c = { sg | count <- c }
        updateSpecies sg s = { sg | name <- s }
    in 
      case fc of
        Count c -> { m | sighting <- (updateCount m.sighting c)}
        Species s -> { m | sighting <- (updateSpecies m.sighting s)}

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