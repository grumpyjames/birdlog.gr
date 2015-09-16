module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import CommonLocator exposing (tiley2lat, tilex2long)
import MapBox exposing (mapBox)
import Metacarpal exposing (index, Metacarpal, InnerEvent, Event(..))
import Osm exposing (openStreetMap)
import Styles exposing (..)
import Tile
import Tuple as T
import Types exposing (FormState, GeoPoint, Model, Position, Tile, TileOffset, TileSource, Zoom, Recording(..), Sighting)
import Ui

import Color exposing (rgb)
import Debug exposing (crash, log)
import Http
import Html exposing (Attribute, Html, button, div, input, form, text, select, option, fromElement)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (Options, onClick, on, onWithOptions, onMouseDown, targetValue)
import Json.Decode as J exposing (Decoder, object2, int, value, (:=), fail)
import Keyboard
import List as L
import Maybe as M
import Result exposing (Result(..))
import Signal as S
import String
import Task exposing (Task)
import Time exposing (Time)
import Window

port hdpi : Bool
port initialWinX : Int
port initialWinY : Int
port location : Signal (Maybe (Float, Float))
port locationError : Signal (Maybe String)

locationRequests = S.mailbox ()
port requestLocation : Signal ()
port requestLocation = locationRequests.signal

-- main
main = 
    let initialZoom = 15.0
        initialWindow = (initialWinX, initialWinY)
        initialMouse = (False, (0,0))
        initialModel = Model hdpi greenwich initialWindow initialZoom initialMouse defaultTileSrc Nothing [] False Nothing
    in S.map view (S.foldp applyEvent initialModel events)

-- a few useful constants
accessToken = "pk.eyJ1IjoiZ3J1bXB5amFtZXMiLCJhIjoiNWQzZjdjMDY1YTI2MjExYTQ4ZWU4YjgwZGNmNjUzZmUifQ.BpRWJBEup08Z9DJzstigvg"
mapBoxSource = mapBox hdpi "mapbox.run-bike-hike" accessToken
defaultTileSrc = mapBoxSource
greenwich = GeoPoint 51.48 0.0        

-- Signal graph and model update
type Events = ZoomChange Float 
            | ArrowPress (Int, Int) 
            | TileSourceChange TileSource 
            | Click (Int, Int)
            | DismissModal
            | TouchEvent (Maybe Event)
            | SightingChange FormChange 
            | R Recording
            | W (Int, Int)
            | N 
            | L (Maybe (Float, Float)) 
            | LocationRequestError (Maybe String) 
            | St

actions : S.Mailbox Events
actions = S.mailbox N

type FormChange = Species String
                | Count String

keyState : Signal (Int, Int)
keyState =
    let toTuple a = (a.x, a.y)
    in S.map toTuple Keyboard.arrows

events : Signal (Time, Events)
events = 
    let win = S.map W <| Window.dimensions
        keys = S.map ArrowPress <| S.map (T.multiply (256, 256)) <| keyState
        ot = S.map TouchEvent index.signal
        ls = S.map L location
        les = S.map LocationRequestError locationError
        lrs = S.sampleOn locationRequests.signal (S.constant St)
    in Time.timestamp <| S.mergeMany [actions.signal, lrs, les, ls, win, keys, ot]

-- Applying events to the model
applyEvent : (Time, Events) -> Model -> Model
applyEvent (t, e) m = case e of
 ZoomChange f -> applyZoom m f
 ArrowPress ap -> applyKeys m ap 
 Click c -> applyClick m t c
 TouchEvent te -> (applyMaybe (applyTouchEvent t)) m te
 SightingChange fc -> applySightingChange m fc
 R r -> applyR m r
 TileSourceChange tsc -> {m | tileSource <- tsc }
 W w -> {m | windowSize <- w}
 St -> {m | locationProgress <- True}
 L l -> applyMaybe (\m (lat, lon) -> {m | centre <- (GeoPoint lat lon), locationProgress <- False}) m l
 LocationRequestError le -> {m | message <- le, locationProgress <- False}
 DismissModal -> { m | message <- Nothing, formState <- Nothing }

applyMaybe : (Model -> a -> Model) -> Model -> Maybe a -> Model
applyMaybe f m maybs = M.withDefault m <| M.map (\j -> f m j) maybs

applyR : Model -> Recording -> Model
applyR m r = 
    { m
    | recordings <- (r :: m.recordings)
    , formState <- Nothing
    }

applyFc' : FormState -> FormChange -> FormState
applyFc' fs fc = 
    case fc of
      Count c -> { fs | count <- c }
      Species s -> { fs | species <- s}
                              
applySightingChange : Model -> FormChange -> Model
applySightingChange m fc =
    let applyFc' fs fc = 
        case fc of
          Count c -> { fs | count <- c }
          Species s -> { fs | species <- s}
    in
      case m.formState of
        Just fs -> { m | formState <- (Just <| applyFc' fs fc) }
        Nothing -> m

applyClick : Model -> Time -> (Int, Int) -> Model
applyClick m t c =
    let newFormState = FormState "" "" (toGeopoint m c) t
    in { m | formState <- Just newFormState }

applyZoom : Model -> Float -> Model
applyZoom m f = { m | zoom <- f + m.zoom }

applyKeys : Model -> (Int, Int) -> Model
applyKeys m k = M.withDefault (applyDrag m k) <| M.map (\t -> m) m.formState

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag = { m | centre <- move m.zoom m.centre drag } 

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff = 
    let (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ (floor z)))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)


applyTouchEvent : Time -> Model -> Event -> Model
applyTouchEvent t m e = 
    case e of
      Metacarpal.Drag pn ->
          applyDrag m ((1, -1) `T.multiply` pn)
      DoubleClick pn ->
          applyClick m t pn
      LongPress pn->
          applyClick m t pn

-- view concerns
view model = 
    let mapLayer = Tile.render model
        window = model.windowSize
        styles = style (absolute ++ dimensions window ++ zeroMargin)
        controls = buttons model [style absolute] actions.address locationRequests.address
-- Really want to pick the first of model.formState and model.message that isn't Nothing.                   
        spottedLayers = spotLayers actions.address model
        recentRecords = records model
        clickCatcher = div (index.attr ++ [styles]) [] 
    in div [styles] ([mapLayer, clickCatcher, controls] ++ recentRecords ++ spottedLayers)

fold : (e -> c) -> (o -> c) -> Result e o -> c
fold f g r = 
    case r of
      Ok o -> g o
      Err e -> f e

spotLayers : S.Address (Events) -> Model -> List Html
spotLayers addr model =
    case model.message of
      Just message -> modalMessage addr model message
      Nothing -> 
          case model.formState of
            Just f -> formLayers addr model f
            Nothing -> []

toSighting : FormState -> Result String Sighting
toSighting fs =
    let countOk = String.toInt fs.count `Result.andThen` (\i -> if i > 0 then (Result.Ok i) else (Result.Err "count must be positive"))
        speciesNonEmpty c = 
            if (String.isEmpty fs.species)
            then (Result.Err "Species not set")
            else (Result.Ok (Sighting c fs.species fs.location fs.time))
    in countOk `Result.andThen` speciesNonEmpty

identity a = a

modalMessage : S.Address (Events) -> Model -> String -> List Html
modalMessage addr m message = 
    let dismissAddr = S.forwardTo addr (\_ -> DismissModal)
        modalContent = div [] [text message, button [on "click" (J.succeed "") (\_ -> S.message addr (DismissModal))] [text "Ok..."]]
    in [Ui.modal dismissAddr m.windowSize modalContent]

-- unwrap the formstate outside
formLayers : S.Address (Events) -> Model -> FormState -> List Html
formLayers addr m formState =
    let cp = fromGeopoint m formState.location
        indicators g = [tick [] (cp `T.add` (2, 2)), tick [("color", "#33AA33")] cp]
        saw = text "Spotted: "
        dismissAddr = S.forwardTo addr (\_ -> DismissModal)
        countDecoder = J.map Count targetValue
        sendFormChange fc = S.message addr (SightingChange fc)
        count = input [ Attr.id "count", Attr.type' "number", Attr.placeholder "Count, e.g 1",  on "change" countDecoder sendFormChange, on "input" countDecoder sendFormChange] []
        speciesDecoder = J.map Species targetValue
        bird = input [ Attr.id "species", Attr.type' "text", Attr.placeholder "Species, e.g Puffin", on "input" speciesDecoder sendFormChange] []
        sighting = toSighting formState
        decoder = J.customDecoder (J.succeed sighting) identity
        disabled = fold (\a -> True) (\b -> False) sighting
        submit = Ui.submitButton decoder (\s -> S.message addr (R (New s))) "Save" disabled
        theForm = form [style [("opacity", "0.8")]] [saw, count, bird, submit]
    in indicators formState.location ++ [Ui.modal dismissAddr m.windowSize theForm]

-- tick!
tick : Style -> (Int, Int) -> Html
tick moreStyle g = 
    let styles = absolute ++ position (g `T.subtract` (6, 20)) ++ zeroMargin
    in div [style (styles ++ moreStyle), Attr.class "tick"] []

-- consolidate records into sightings
sightings : List Recording -> List Sighting
sightings rs = L.map (\r -> case r of New s -> s) rs

records : Model -> List Html
records model =
    L.map (\g -> tick [] (fromGeopoint model g)) <| L.map (\s -> s.location) <| sightings model.recordings

ons : S.Address (Events) -> Attribute
ons add = let 
    toMsg v = case v of
                "OpenStreetMap" -> openStreetMap
                "ArcGIS" -> arcGIS
                "MapBox" -> mapBoxSource
    in on "change" targetValue (\v -> S.message add (TileSourceChange (toMsg v)))

tileSrcDropDown : S.Address (Events) -> Html
tileSrcDropDown address = 
    let onChange = ons address
    in select [onChange] [option [] [text "MapBox"], option [] [text "OpenStreetMap"], option [] [text "ArcGIS"]]                

zoomIn address = ourButton [("circ", True), ("zoom", True)] address (ZoomChange 1) "+"
zoomOut address = ourButton [("circ", True), ("zoom", True)] address (ZoomChange (-1)) "-"
locationButton inProgress address = ourButton [("circ", True), ("location", True), ("inprogress", inProgress)] address () ""

buttons model attrs actionAddress locationRequestAddress = 
    div attrs [zoomIn actionAddress, zoomOut actionAddress, locationButton model.locationProgress locationRequestAddress, tileSrcDropDown actionAddress]

hoverC = rgb 240 240 240
downC = rgb 235 235 235
upC = rgb 248 248 248

ourButton : List (String, Bool) -> (S.Address a) -> a -> String -> Html
ourButton classes address msg txt = 
    let events = [onMouseDown, onClick, (\ad ms -> on "touchend" value (\_ -> Signal.message ad ms))]
    in button ((Attr.classList classes) :: (L.map (\e -> e address msg) events)) [text txt]

-- lon min: -180
-- lat min : 85.0511

toPixels : Int -> TileOffset -> (Int, Int)
toPixels tileSize tileOff = 
    let fromTile = T.map ((*) tileSize) tileOff.tile.coordinate
        fromPixels = tileOff.position.pixels
    in fromTile `T.add` fromPixels

diff : TileOffset -> TileOffset -> TileOffset
diff to1 to2 =
    let posDiff = Position <| to1.position.pixels `T.subtract` to2.position.pixels
        tileDiff = Tile <| to1.tile.coordinate `T.subtract` to2.tile.coordinate
    in TileOffset tileDiff posDiff

toGeopoint : Model -> (Int, Int) -> GeoPoint
toGeopoint model clk = 
    let win = model.windowSize
        centre = model.centre
        tileSize = model.tileSource.tileSize
        centrePix = toPixels tileSize <| model.tileSource.locate model.zoom model.centre
        middle = T.map (\a -> a // 2) win
        clickPix = T.map (\x -> x / (toFloat tileSize)) <| T.map toFloat <| (clk `T.subtract` middle) `T.add` centrePix
    in GeoPoint (tiley2lat (snd clickPix) model.zoom) (tilex2long (fst clickPix) model.zoom)

fromGeopoint : Model -> GeoPoint -> (Int, Int)
fromGeopoint model loc =
    let win = model.windowSize
        centreOffPx = toPixels model.tileSource.tileSize <| model.tileSource.locate model.zoom model.centre
        tileOffPx = toPixels model.tileSource.tileSize <| model.tileSource.locate model.zoom loc        
        pixCentre = T.map (\x -> x // 2) win
        offPix = (tileOffPx `T.subtract` centreOffPx)
    in pixCentre `T.add` offPix