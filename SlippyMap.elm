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
        initialModel = Model hdpi greenwich initialWindow initialZoom initialMouse defaultTileSrc Nothing [] False
    in S.map view (S.foldp applyEvent initialModel events)

-- a few useful constants
accessToken = "pk.eyJ1IjoiZ3J1bXB5amFtZXMiLCJhIjoiNWQzZjdjMDY1YTI2MjExYTQ4ZWU4YjgwZGNmNjUzZmUifQ.BpRWJBEup08Z9DJzstigvg"
mapBoxSource = mapBox hdpi "mapbox.run-bike-hike" accessToken
defaultTileSrc = mapBoxSource
greenwich = GeoPoint 51.48 0.0        

-- Signal graph and model update
type Events = Z ZoomChange | K (Int, Int) | T TileSource | C (Maybe (Int, Int)) | O (Maybe Event) | F FormChange | R Recording | W (Int, Int) | N | L (Maybe (Float, Float)) | St

actions : S.Mailbox Events
actions = S.mailbox N

type FormChange = Species String
                | Count String

type ZoomChange = In Float | Out Float

keyState : Signal (Int, Int)
keyState =
    let toTuple a = (a.x, a.y)
    in S.map toTuple Keyboard.arrows

events : Signal (Time, Events)
events = 
    let win = S.map W <| Window.dimensions
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        ot = S.map O index.signal
        ls = S.map L location
        lrs = S.sampleOn locationRequests.signal (S.constant St)
    in Time.timestamp <| S.mergeMany [actions.signal, lrs, ls, win, keys, ot]

-- Applying events to the model
applyEvent : (Time, Events) -> Model -> Model
applyEvent (t, e) m = case e of
 Z zo -> applyZoom m zo
 K ke -> applyKeys m ke 
 C c -> applyClick m t c
 O o -> (applyMaybe (applyO t)) m o
 F fc -> applyFc m fc
 R r -> applyR m r
 T ti -> {m | tileSource <- ti }
 W w -> {m | windowSize <- w}
 St -> {m | locationProgress <- True}
 L l -> applyMaybe (\m (lat, lon) -> {m | centre <- (GeoPoint lat lon), locationProgress <- False}) m l

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
                              
applyFc : Model -> FormChange -> Model
applyFc m fc = 
    case m.formState of
      Just fs -> { m | formState <- (Just <| applyFc' fs fc) }
      Nothing -> m

applyClick : Model -> Time -> Maybe (Int, Int) -> Model
applyClick m t c =
    let newFormState = M.map (\cp -> FormState "" "" (toGeopoint m cp) t) c
    in { m | formState <- newFormState }

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = 
    let 
        newZoom zc z =
            case zc of
              In a -> z + a
              Out a -> z - a
    in { m | zoom <- newZoom zc m.zoom }

applyKeys : Model -> (Int, Int) -> Model
applyKeys m k = M.withDefault (applyDrag m k) <| M.map (\t -> m) m.formState

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag = { m | centre <- move m.zoom m.centre drag } 

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff = 
    let (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ (floor z)))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)


applyO : Time -> Model -> Event -> Model
applyO t m e = 
    case e of
      Metacarpal.Drag pn ->
          applyDrag m ((1, -1) `T.multiply` pn)
      DoubleClick pn ->
          applyClick m t (Just pn)
      LongPress pn->
          applyClick m t (Just pn)

-- view concerns
view model = 
    let mapLayer = Tile.render model
        window = model.windowSize
        styles = style (absolute ++ dimensions window ++ zeroMargin)
        controls = buttons model [style absolute] actions.address locationRequests.address
        spottedLayers = spotLayers actions.address model
        recentRecords = records model
        dblClick = index.attr
        clickCatcher = div (dblClick ++ [styles]) []
    in div [styles] ([mapLayer, clickCatcher, controls] ++ recentRecords ++ spottedLayers)

targetId : Decoder String
targetId = ("target" := ("id" := J.string))        

isTargetId : String -> Decoder Bool
isTargetId id = J.customDecoder targetId (\eyed -> if eyed == id then Result.Ok True else Result.Err "nope!") 

targetWithId : (Bool -> S.Message) -> String -> String -> Attribute
targetWithId msg event id = on event (isTargetId id) msg

fold : (e -> c) -> (o -> c) -> Result e o -> c
fold f g r = 
    case r of
      Ok o -> g o
      Err e -> f e

spotLayers : S.Address (Events) -> Model -> List Html
spotLayers addr model =
    case model.formState of
      Just f -> formLayers addr model f
      Nothing -> []

toSighting : FormState -> Result String Sighting
toSighting fs =
    let countOk = String.toInt fs.count
        speciesNonEmpty c = 
            if (String.isEmpty fs.species)
            then (Result.Err "Species not set")
            else (Result.Ok (Sighting c fs.species fs.location fs.time))
    in countOk `Result.andThen` speciesNonEmpty

identity a = a

-- unwrap the formstate outside
formLayers : S.Address (Events) -> Model -> FormState -> List Html
formLayers addr m formState =
    let cp = fromGeopoint m formState.location
        indicators g = [tick [] (cp `T.add` (2, 2)), tick [("color", "#33AA33")] cp]
        modalId = "modal"
        cancel = targetWithId (\_ -> S.message addr (C Nothing)) "click" modalId
        saw = text "Spotted: "
        countDecoder = J.map Count targetValue
        sendFormChange fc = S.message addr (F fc)
        count = input [ Attr.id "count", Attr.type' "number", Attr.placeholder "1",  on "change" countDecoder sendFormChange, on "input" countDecoder sendFormChange] []
        speciesDecoder = J.map Species targetValue
        bird = input [ Attr.id "species", Attr.type' "text", Attr.placeholder "Puffin", on "input" speciesDecoder sendFormChange] []
        sighting = toSighting formState
        decoder = J.customDecoder (J.succeed sighting) identity
        disabled = fold (\a -> True) (\b -> False) sighting
        submit = Ui.submitButton decoder (\s -> S.message addr (R (New s))) "Save" disabled
        theForm = form [style [("opacity", "0.8")]] [saw, count, bird, submit]
    in indicators formState.location ++ [Ui.modal [Attr.id modalId, cancel] m.windowSize theForm]

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
    in on "change" targetValue (\v -> S.message add (T (toMsg v)))

tileSrcDropDown : S.Address (Events) -> Html
tileSrcDropDown address = 
    let onChange = ons address
    in select [onChange] [option [] [text "MapBox"], option [] [text "OpenStreetMap"], option [] [text "ArcGIS"]]                

zoomIn address = ourButton [("circ", True), ("zoom", True)] address (Z (In 1)) "+"
zoomOut address = ourButton [("circ", True), ("zoom", True)] address (Z (Out 1)) "-"
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