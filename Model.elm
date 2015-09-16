module Model (Events(..), FormChange(..), FormState, Model, Recording(..), Sighting, applyEvent) where

import CommonLocator
import Metacarpal
import Tuple as T
import Types exposing (GeoPoint, Locator, TileSource, TileUrl, Zoom)

import Maybe as M
import Time exposing (Time)

type Recording = 
    New Sighting

type Events = ZoomChange Float 
            | ArrowPress (Int, Int) 
            | TileSourceChange TileSource 
            | Click (Int, Int)
            | DismissModal
            | TouchEvent (Maybe Metacarpal.Event)
            | SightingChange FormChange 
            | RecordChange Recording
            | WindowSize (Int, Int)
            | StartingUp 
            | LocationReceived (Maybe (Float, Float)) 
            | LocationRequestError (Maybe String) 
            | LocationRequestStarted

type FormChange = Species String
                | Count String

type alias FormState =
    {
      count: String
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
    , formState : Maybe FormState
    , recordings : List Recording
    , locationProgress : Bool
    , message : Maybe String
    }

type alias Sighting =
    {
      count: Int
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
      SightingChange fc -> applySightingChange m fc
      RecordChange r -> applyRecordChange m r
      TileSourceChange tsc -> {m | tileSource <- tsc }
      WindowSize w -> {m | windowSize <- w}
      LocationRequestStarted -> {m | locationProgress <- True}
      LocationReceived l -> applyMaybe (\m (lat, lon) -> {m | centre <- (GeoPoint lat lon), locationProgress <- False}) m l
      LocationRequestError le -> {m | message <- le, locationProgress <- False}
      DismissModal -> { m | message <- Nothing, formState <- Nothing }
      StartingUp -> m

applyMaybe : (Model -> a -> Model) -> Model -> Maybe a -> Model
applyMaybe f m maybs = M.withDefault m <| M.map (\j -> f m j) maybs

applyRecordChange : Model -> Recording -> Model
applyRecordChange m r = 
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

applyTouchEvent : Time -> Model -> Metacarpal.Event -> Model
applyTouchEvent t m e = 
    case e of
      Metacarpal.Drag pn ->
          applyDrag m ((1, -1) `T.multiply` pn)
      Metacarpal.DoubleClick pn ->
          applyClick m t pn
      Metacarpal.LongPress pn->
          applyClick m t pn


toGeopoint : Model -> (Int, Int) -> GeoPoint
toGeopoint model clk = 
    let win = model.windowSize
        centre = model.centre
        tileSize = model.tileSource.tileSize
        centrePix = CommonLocator.toPixels tileSize <| model.tileSource.locate model.zoom model.centre
        middle = T.map (\a -> a // 2) win
        clickPix = T.map (\x -> x / (toFloat tileSize)) <| T.map toFloat <| (clk `T.subtract` middle) `T.add` centrePix
    in GeoPoint (CommonLocator.tiley2lat (snd clickPix) model.zoom) (CommonLocator.tilex2long (fst clickPix) model.zoom)
