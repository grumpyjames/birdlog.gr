module Model (Events(..), FormChange(..), FormState, Model, Recording(..), Sighting, SightingForm(..), applyEvent, state) where

import CommonLocator
import Metacarpal
import Tuple as T
import Types exposing (GeoPoint, Locator, TileSource, TileUrl, Zoom(..))

import List as L
import Maybe as M
import Time exposing (Time)

type Recording = New Sighting
               | Amend Sighting

type Events = ZoomChange Float 
            | ArrowPress (Int, Int) 
            | TileSourceChange TileSource 
            | Click (Int, Int)
            | AmendRecord Int
            | DismissModal
            | TouchEvent (Maybe Metacarpal.Event)
            | SightingChange FormChange 
            | RecordChange Recording
            | WindowSize (Int, Int)
            | StartingUp 
            | LocationReceived (Maybe (Float, Float)) 
            | LocationRequestError (Maybe String) 
            | LocationRequestStarted
            | LayerReady Int

type FormChange = Species String
                | Count String


type SightingForm = PendingAmend FormState
                  | JustSeen FormState
                  | Amending FormState

state : SightingForm -> FormState
state sf = 
    case sf of
      PendingAmend fs -> fs
      JustSeen fs -> fs
      Amending fs -> fs

type alias FormState =
    {
      id: Int
    , count: String
    , species: String
    , location: GeoPoint
    , time: Time 
    }

type alias Model = 
    { 
      hdpi : Bool
    , centre : GeoPoint
    , windowSize : (Int, Int)
    , zoom : Zoom
    , mouseState : (Bool, (Int, Int))
    , tileSource : TileSource
    , formState : Maybe SightingForm
    , recordings : List Recording
    , locationProgress : Bool
    , message : Maybe String
    , nextId : Int
    }

type alias Sighting =
    {
      id: Int
    , count: Int
    , species: String
    , location: GeoPoint
    -- millis
    , time : Time
    }

applyEvent : (Time, Events) -> Model -> Model
applyEvent (t, e) m = 
    case e of
      ZoomChange f -> applyZoom m f
      ArrowPress ap -> applyKeys m ap 
      Click c -> applyClick m t c
      TouchEvent te -> (applyMaybe (applyTouchEvent t)) m te
      SightingChange fc -> {m | formState <- (M.map (\fs -> applyFormChange fs fc) m.formState)}
      RecordChange r -> applyRecordChange m r
      TileSourceChange tsc -> {m | tileSource <- tsc }
      WindowSize w -> {m | windowSize <- w}
      LocationRequestStarted -> {m | locationProgress <- True}
      LocationReceived l -> applyMaybe (\m (lat, lon) -> {m | centre <- (GeoPoint lat lon), locationProgress <- False}) m l
      LocationRequestError le -> {m | message <- le, locationProgress <- False}
      DismissModal -> { m | message <- Nothing, formState <- Nothing }
      AmendRecord id -> prepareToAmend m id
      LayerReady lr -> maybeUpdateZoom m lr
      StartingUp -> m

applyMaybe : (b -> a -> b) -> b -> Maybe a -> b
applyMaybe f b maybs = M.withDefault b <| M.map (\j -> f b j) maybs

maybeUpdateZoom : Model -> Int -> Model
maybeUpdateZoom m readyLevel =
    case m.zoom of
      Constant c -> m
      Between a b -> if b == readyLevel then { m | zoom <- Constant readyLevel } else m

applyRecordChange : Model -> Recording -> Model
applyRecordChange m r = 
    { m
    | recordings <- (r :: m.recordings)
    , formState <- Nothing
    }

applyFormChange : SightingForm -> FormChange -> SightingForm
applyFormChange sf fc =
    let newFormState fs fc = 
        case fc of
          Count c -> { fs | count <- c }
          Species s -> { fs | species <- s}
    in 
      case sf of
        PendingAmend fs -> Amending (newFormState fs fc)
        Amending fs -> Amending (newFormState fs fc)
        JustSeen fs -> JustSeen (newFormState fs fc)
 
applyClick : Model -> Time -> (Int, Int) -> Model
applyClick m t c =
    let newFormState = FormState m.nextId "" "" (toGeopoint m c) t
        nextNextId = m.nextId + 1
    in { m | formState <- Just (JustSeen newFormState), nextId <- nextNextId }

newZoom : Zoom -> Float -> Zoom
newZoom z f =
    let 
        intF = floor f
        zm a b = if (a == b) then Constant a else Between a b
    in 
      case z of
        Constant c -> Between c (c + intF)
        Between from to -> zm from (to + intF)
           

applyZoom : Model -> Float -> Model
applyZoom m f = { m | zoom <- newZoom m.zoom f }

applyKeys : Model -> (Int, Int) -> Model
applyKeys m k = 
    case m.formState of
      Nothing -> applyDrag m k
      otherwise -> m

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag = { m | centre <- move m.zoom m.centre drag } 

pickZoom : Zoom -> Int
pickZoom z = 
    case z of
      Constant c -> c
      Between a b -> b

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff = 
    let zoomInt = pickZoom z
        (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ zoomInt))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

applyTouchEvent : Time -> Model -> Metacarpal.Event -> Model
applyTouchEvent t m e = 
    case e of
      Metacarpal.Drag pn ->
          applyDrag m ((1, -1) `T.multiply` pn)
      Metacarpal.DoubleClick pn ->
          applyClick m t pn
      Metacarpal.LongPress pn->
          applyClick m t pn

prepareToAmend : Model -> Int -> Model
prepareToAmend m id =
    let pred r =
            case r of 
              New s -> s.id == id
              Amend s -> s.id == id
        record = findLast pred m.recordings
    in {m | formState <- fromRecord record}

fromRecord : Maybe Recording -> Maybe SightingForm
fromRecord record =
    let s r = 
        case r of 
          New sighting -> sighting
          Amend sighting -> sighting
        fs s = FormState s.id (toString s.count) s.species s.location s.time
    in M.map PendingAmend <| M.map fs <| M.map s record

findLast : (a -> Bool) -> List a -> Maybe a
findLast pred l = 
    let foldFn a b = if pred a then Just a else b
    in L.foldr foldFn Nothing l

toGeopoint : Model -> (Int, Int) -> GeoPoint
toGeopoint model clk = 
    let win = model.windowSize
        centre = model.centre
        tileSize = model.tileSource.tileSize
        z = toFloat <| pickZoom model.zoom
        centrePix = CommonLocator.toPixels tileSize <| model.tileSource.locate z model.centre
        middle = T.map (\a -> a // 2) win
        clickPix = T.map (\x -> x / (toFloat tileSize)) <| T.map toFloat <| (clk `T.subtract` middle) `T.add` centrePix
    in GeoPoint (CommonLocator.tiley2lat (snd clickPix) z) (CommonLocator.tilex2long (fst clickPix) z)
