module Model ( Events(..)
             , FormChange(..)
             , FormState
             , ModalMessage(..)
             , Model
             , Recording(..)
             , ReplicationState(..)
             , Sequenced
             , SessionState(..)
             , Sighting
             , SightingForm(..)
             , applyEvent
             , state) where

import CommonLocator
import Metacarpal
import Tuple as T
import Types exposing (GeoPoint, Locator, TileSource, TileUrl, Zoom(..))

import Debug
import List as L
import Maybe as M
import Time exposing (Time)

type alias Sequenced a = 
    { sequence: Int
    , item : a
    }

type Recording = New Sighting
               | Replace Int Sighting
               | Delete Int

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
            | LayerReady (Int, Float)
            | Replicate (List (Sequenced Recording))
            | HighWaterMark Int
            | Pulse Time
            | LoggedIn String Int

type FormChange = Species String
                | Count String

type SightingForm = PendingAmend Int FormState
                  | JustSeen FormState
                  | Amending Int FormState
                  | Deleting FormState

state : SightingForm -> FormState
state sf = 
    case sf of
      PendingAmend seq fs -> fs
      JustSeen fs -> fs
      Amending seq fs -> fs

type alias FormState =
    {
      count: String
    , species: String
    , location: GeoPoint
    , time: Time 
    }

type ReplicationState = ReplicatingSince Time
                      | ReplicatedAt Time

type ModalMessage = Message String
                  | Instructions

type SessionState = NotLoggedIn
                  | LoggedInUser String

type alias Model = 
    { 
      hdpi : Bool
    , centre : GeoPoint
    , windowSize : (Int, Int)
    , zoom : Zoom
    , mouseState : (Bool, (Int, Int))
    , tileSource : TileSource
    , formState : Maybe SightingForm
    , records : List (Sequenced Recording)
    , locationProgress : Bool
    , message : Maybe ModalMessage 
    , nextSequence : Int
    -- records with sequence <= than this have been synced with the server
    , highWaterMark : Int
    , replicationState : ReplicationState
    , lastPulseTime : Time
    , sessionState : SessionState
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
      SightingChange fc -> {m | formState <- (M.map (\fs -> applyFormChange fs fc) m.formState)}
      RecordChange r -> applyRecordChange m r
      TileSourceChange tsc -> {m | tileSource <- tsc }
      WindowSize w -> {m | windowSize <- w}
      LocationRequestStarted -> {m | locationProgress <- True}
      LocationReceived l -> applyMaybe (\m (lat, lon) -> {m | centre <- (GeoPoint lat lon), locationProgress <- False}) m l
      LocationRequestError le -> {m | message <- (M.map Message le), locationProgress <- False}
      DismissModal -> { m | message <- Nothing, formState <- Nothing }
      AmendRecord sequence -> prepareToAmend m sequence
      LayerReady lr -> maybeUpdateZoom m lr
      HighWaterMark hwm -> { m | highWaterMark <- hwm, replicationState <- ReplicatedAt t }
      Pulse t -> { m | lastPulseTime <- t }
      LoggedIn nick lastSeq -> { m | sessionState <- LoggedInUser nick, nextSequence <- 1 + lastSeq }
      otherwise -> m

applyMaybe : (b -> a -> b) -> b -> Maybe a -> b
applyMaybe f b maybs = M.withDefault b <| M.map (\j -> f b j) maybs

maybeUpdateZoom : Model -> (Int, Float) -> Model
maybeUpdateZoom m (readyLevel, progressIncrement) =
    case m.zoom of
      Constant c -> m
      Between a b p -> 
          if b == readyLevel 
          then
              if (p + progressIncrement) > 0.75
              then { m | zoom <- Debug.log "transition complete" (Constant b) }
              else { m | zoom <- Debug.log "updating progress" (Between a b (p + progressIncrement)) }
          else
              m

applyRecordChange : Model -> Recording -> Model
applyRecordChange m r = 
    let newSequence = m.nextSequence + 1
    in
      { m
      | records <- (Sequenced m.nextSequence r) :: m.records
      , formState <- Nothing
      , nextSequence <- newSequence
      }

applyFormChange : SightingForm -> FormChange -> SightingForm
applyFormChange sf fc =
    let newFormState fs fc = 
        case fc of
          Count c -> { fs | count <- c }
          Species s -> { fs | species <- s}
    in 
      case sf of
        PendingAmend seq fs -> Amending seq (newFormState fs fc)
        Amending seq fs -> Amending seq (newFormState fs fc)
        JustSeen fs -> JustSeen (newFormState fs fc)
 
applyClick : Model -> Time -> (Int, Int) -> Model
applyClick m t c =
    let newFormState = FormState "" "" (toGeopoint m c) t
    in { m | formState <- Just (JustSeen newFormState) }

newZoom : Zoom -> Float -> Zoom
newZoom z f =
    let 
        intF = floor f
        zm a b = if (a == b) then Constant a else Between a b 0.0
    in 
      case z of
        Constant c -> Between c (c + intF) 0.0
        Between from to p -> Debug.log "switching to zoom" <| zm from (to + intF)
           

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
      Between a b p -> b

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
prepareToAmend m seq =
    let pred r = (r.sequence == seq)
        record = findLast pred m.records
    in {m | formState <- fromRecord record}

toFormState : (Sequenced Recording) -> Maybe SightingForm
toFormState r = 
    case r.item of 
      Delete seq -> Nothing
      New s -> Just <| PendingAmend r.sequence <| FormState (toString s.count) s.species s.location s.time
      Replace seq s -> Just <| PendingAmend r.sequence <| FormState (toString s.count) s.species s.location s.time

fromRecord : Maybe (Sequenced Recording) -> Maybe SightingForm
fromRecord record = record `M.andThen` toFormState

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
