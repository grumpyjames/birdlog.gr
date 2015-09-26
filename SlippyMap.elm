module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import CommonLocator exposing (tiley2lat, tilex2long)
import MapBox exposing (mapBox)
import Metacarpal exposing (index, Metacarpal, InnerEvent, Event(..))
import Model exposing (ModalState(..), Events(..), FormChange(..), FormState, Model, Recording(..), Sighting, SightingForm(..), applyEvent, state)
import Osm exposing (openStreetMap)
import Styles exposing (..)
import Tile
import Tuple as T
import Types exposing (GeoPoint, Position, Tile, TileOffset, TileSource, Zoom(..))
import Ui

import Color exposing (rgb)
import Debug exposing (crash, log)
import Dict as D
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
    let initialZoom = Constant 15
        initialWindow = (initialWinX, initialWinY)
        initialMouse = (False, (0,0))
        initialModel = Model hdpi greenwich initialWindow initialZoom initialMouse defaultTileSrc [] False Nothing 0
    in S.map view (S.foldp applyEvent initialModel events)

-- a few useful constants
accessToken = "pk.eyJ1IjoiZ3J1bXB5amFtZXMiLCJhIjoiNWQzZjdjMDY1YTI2MjExYTQ4ZWU4YjgwZGNmNjUzZmUifQ.BpRWJBEup08Z9DJzstigvg"
mapBoxSource = mapBox hdpi "mapbox.run-bike-hike" accessToken
defaultTileSrc = mapBoxSource
greenwich = GeoPoint 51.48 0.0        

-- Signal graph and model update

actions : S.Mailbox Events
actions = S.mailbox StartingUp

keyState : Signal (Int, Int)
keyState =
    let toTuple a = (a.x, a.y)
    in S.map toTuple Keyboard.arrows

events : Signal (Time, Events)
events = 
    let win = S.map WindowSize <| Window.dimensions
        keys = S.map ArrowPress <| S.map (T.multiply (256, 256)) <| keyState
        ot = S.map TouchEvent index.signal
        ls = S.map LocationReceived location
        les = S.map LocationRequestError locationError
        lrs = S.sampleOn locationRequests.signal (S.constant LocationRequestStarted)
    in Time.timestamp <| S.mergeMany [actions.signal, lrs, les, ls, win, keys, ot]

-- view concerns
view model = 
    let layerReady = S.forwardTo actions.address (\r -> LayerReady r)
        mapLayer = Tile.render layerReady model
        window = model.windowSize
        styles = style (absolute ++ dimensions window ++ zeroMargin)
        controls = buttons model [style absolute] actions.address locationRequests.address
        spottedLayers = spotLayers actions.address model
        recentRecords = records actions.address model
        clickCatcher = div (index.attr ++ [styles]) [] 
    in div [styles] ([mapLayer, clickCatcher, controls] ++ recentRecords ++ spottedLayers)

mapError : (e1 -> e2) -> Result e1 a -> Result e2 a
mapError g r = 
    case r of
      Ok o -> Ok o
      Err e -> Err (g e)

fold : (e -> c) -> (o -> c) -> Result e o -> c
fold f g r = 
    case r of
      Ok o -> g o
      Err e -> f e

spotLayers : S.Address (Events) -> Model -> List Html
spotLayers addr model =
    case model.modalState of
      Nothing -> []
      Just ms ->
          case ms of
            Msg msg -> modalMessage addr model msg
            Form sf -> formLayers addr model sf
 
toSighting : SightingForm -> Result String Recording
toSighting sf =
    let countOk fs = (mapError (\a -> "count must be positive") (String.toInt fs.count)) `Result.andThen` (\i -> if i > 0 then (Result.Ok i) else (Result.Err "count must be positive"))
        speciesNonEmpty fs c = 
            if (String.isEmpty fs.species)
            then (Result.Err "Species not set")
            else (Result.Ok (Sighting fs.id c fs.species fs.location fs.time))
        validate fs = (countOk fs) `Result.andThen` (speciesNonEmpty fs)
    in case sf of
         JustSeen fs -> Result.map New (validate fs)
         Amending fs -> Result.map Amend (validate fs)
         PendingAmend fs -> Result.map Amend (validate fs)

identity a = a

modalMessage : S.Address (Events) -> Model -> String -> List Html
modalMessage addr m message = 
    let dismissAddr = S.forwardTo addr (\_ -> DismissModal)
        modalContent = div [] [text message, button [on "click" (J.succeed "") (\_ -> S.message addr (DismissModal))] [text "Ok..."]]
    in [Ui.modal dismissAddr m.windowSize modalContent]

-- unwrap the formstate outside
formLayers : S.Address (Events) -> Model -> SightingForm -> List Html
formLayers addr m sf =
    let cp = fromGeopoint m ((state sf).location)
        indicators g = [tick [] [] (cp `T.add` (2, 2)), tick [] [("color", "#33AA33")] cp]
        saw = text "Spotted: "
        dismissAddr = S.forwardTo addr (\_ -> DismissModal)
        countDecoder = J.map Count targetValue
        sendFormChange fc = S.message addr (SightingChange fc)
        count = input ((countVal sf) ++ [ Attr.id "count", Attr.type' "number", Attr.placeholder "Count, e.g 7",  on "change" countDecoder sendFormChange, on "input" countDecoder sendFormChange]) []
        speciesDecoder = J.map Species targetValue
        bird = input ((speciesVal sf) ++ [ Attr.id "species", Attr.type' "text", Attr.placeholder "Species, e.g Puffin", on "input" speciesDecoder sendFormChange]) []
        sighting = Result.map RecordChange <| toSighting sf
        decoder = J.customDecoder (J.succeed sighting) identity
        disabled = fold (\a -> True) (\b -> False) sighting
        deleteButton = delButton addr sf "Delete"
        submit = Ui.submitButton decoder (S.message addr) "Save" disabled
        err s = fold (\e -> [(div [Attr.class "error"] [text ("e: " ++ e)])]) (\b -> []) s
        theForm = form [style [("opacity", "0.8")]] ([saw, br, count, br, bird, br] ++ (err sighting) ++ [submit] ++ deleteButton)
    in indicators (state sf).location ++ [Ui.modal dismissAddr m.windowSize theForm]

br = Html.br [] []

delButton : S.Address (Events) -> SightingForm -> String -> List Html
delButton addr sf title = 
    case sf of 
      Amending fs -> [br, Ui.submitButton (J.succeed (RecordChange (Delete fs.id))) (S.message addr) title False]
      PendingAmend pa -> [br, Ui.submitButton (J.succeed (RecordChange (Delete pa.id))) (S.message addr) title False]
      otherwise -> []

val : (FormState -> String) -> SightingForm -> List Attribute
val extractor sf = 
    case sf of
      PendingAmend fs -> [Attr.value (extractor fs)]
      otherwise -> []

speciesVal : SightingForm -> List Attribute
speciesVal = val (\fs -> fs.species)  

countVal : SightingForm -> List Attribute
countVal = val (\fs -> fs.count)

-- tick!
tick : List Attribute -> Style -> (Int, Int) -> Html
tick attrs moreStyle g = 
    let styles = absolute ++ position (g `T.subtract` (6, 20)) ++ zeroMargin
    in div (attrs ++ [style (styles ++ moreStyle), Attr.class "tick"]) []

-- consolidate records into sightings
sightings : List Recording -> List Sighting
sightings rs = 
    let f r d = 
        case r of
          New s -> D.insert s.id s d
          Amend s -> D.insert s.id s d
          Delete id -> D.remove id d
    in D.values <| L.foldr f D.empty (Debug.log "recordings" rs)

records : S.Address (Events) -> Model -> List Html
records addr model =
    let amendAction s = on "click" (J.succeed s.id) (\id -> S.message addr (AmendRecord id)) 
    in L.map (\s -> tick [amendAction s] [] (fromGeopoint model s.location)) <| sightings model.recordings

ons : S.Address (Events) -> Attribute
ons add = 
    let toMsg v = 
        case v of
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
    let events = [onClick, (\ad ms -> onWithOptions "touchend" Ui.stopEverything value (\_ -> Signal.message ad ms))]
    in button ((Attr.classList classes) :: (L.map (\e -> e address msg) events)) [text txt]

-- lon min: -180
-- lat min : 85.0511
diff : TileOffset -> TileOffset -> TileOffset
diff to1 to2 =
    let posDiff = Position <| to1.position.pixels `T.subtract` to2.position.pixels
        tileDiff = Tile <| to1.tile.coordinate `T.subtract` to2.tile.coordinate
    in TileOffset tileDiff posDiff

pickZoom : Zoom -> Int
pickZoom zoom = 
    case zoom of
      Constant c -> c
      Between a b p -> b

fromGeopoint : Model -> GeoPoint -> (Int, Int)
fromGeopoint model loc =
    let win = model.windowSize
        z = toFloat <| pickZoom model.zoom
        centreOffPx = CommonLocator.toPixels model.tileSource.tileSize <| model.tileSource.locate z model.centre
        tileOffPx = CommonLocator.toPixels model.tileSource.tileSize <| model.tileSource.locate z loc        
        pixCentre = T.map (\x -> x // 2) win
        offPix = (tileOffPx `T.subtract` centreOffPx)
    in pixCentre `T.add` offPix