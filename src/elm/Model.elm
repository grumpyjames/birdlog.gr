module Model ( Events(..)
             , FormChange(..)
             , FormState
             , ModalMessage(..)
             , Model
             , ReplicationState(..)
             , SessionState(..)
             , Sighting
             , SightingForm(..)
             , applyEvent
             , state) where

import CommonLocator
import Metacarpal
import Sequenced exposing (Recording(..), Sequenced)
import Tuple as T
import Types exposing (GeoPoint, Locator, TileSource, TileUrl, Zoom(..))

import Debug
import List as L
import Maybe as M
import Time exposing (Time)

type Events = ZoomChange Float 
            | ArrowPress (Int, Int) 
            | TileSourceChange TileSource 
            | Click (Int, Int)
            | AmendRecord Int
            | DismissModal
            | TouchEvent Metacarpal.Event
            | SightingChange FormChange 
            | RecordChange (Recording Sighting)
            | WindowSize (Int, Int)
            | StartingUp 
            | LocationReceived (Maybe (Float, Float)) 
            | LocationRequestError (Maybe String) 
            | LocationRequestStarted
            | LayerReady (Int, Float)
            | Replicate (List (Sequenced (Recording Sighting)))
            | HighWaterMark Int
            | ReplicationFailed
            | Pulse Time
            | LoggedIn String Int (List (Sequenced (Recording Sighting)))
            | ShowInstructions

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

type ReplicationState = Replicating
                      | TriggerReplication (List (Sequenced (Recording Sighting)))
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
    , records : List (Sequenced (Recording Sighting))
    , locationProgress : Bool
    , message : Maybe ModalMessage 
    -- records with sequence <= than this have been synced with the server
    , highWaterMark : Int
    , replicationState : ReplicationState
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

toReplicate : Model -> (List (Sequenced (Recording Sighting)))
toReplicate m = 
    let pred r = r.sequence > m.highWaterMark 
    in L.filter pred m.records 

applyTime : Model -> Time -> Model
applyTime oldM t = 
    case oldM.replicationState of
      ReplicatedAt time -> 
          if time + 5000 < t 
          then 
              let payload = toReplicate oldM
              in if (L.isEmpty payload) 
                 then { oldM | replicationState <- ReplicatedAt t }
                 else 
                     case oldM.sessionState of
                       LoggedInUser n -> { oldM | replicationState <- TriggerReplication payload }
                       otherwise -> oldM
          else oldM
      TriggerReplication _ -> { oldM | replicationState <- Replicating }
      otherwise -> oldM
                            
applyEvent : (Time, Events) -> Model -> Model
applyEvent (t, e) oldM =
    -- allow only a single frame of 'TriggerReplication'
    let m = applyTime oldM t
    in case e of
      ZoomChange f -> applyZoom m f
      ArrowPress ap -> applyKeys m ap 
      Click c -> applyClick m t c
      TouchEvent te -> applyTouchEvent t m te
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
      ReplicationFailed -> { m | replicationState <- ReplicatedAt t }
      Pulse t -> m
      LoggedIn nick hwm rec -> { m | sessionState <- LoggedInUser nick, records <- rec, highWaterMark <- hwm }
      ShowInstructions -> { m | message <- Just Instructions } 
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

any : (a -> Bool) -> List a -> Bool
any p l = L.filter p l |> L.head |> M.map (\a -> True) |> M.withDefault False

applyRecordChange : Model -> (Recording Sighting) -> Model
applyRecordChange m r = 
    let sequence = M.withDefault 0 <| M.map (\s -> s.sequence + 1) <| L.head m.records
        appendRecord rec = 
            { m | records <- (Sequenced sequence rec) :: m.records
            , formState <- Nothing }
    in case r of
         Replace sequence sighting -> 
             let identical = any (Sequenced.matches sequence sighting) m.records
             in if identical
                then { m | formState <- Nothing }
                else appendRecord r
         otherwise -> appendRecord r
              
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

toFormState : (Sequenced (Recording Sighting)) -> Maybe SightingForm
toFormState r = 
    case r.item of 
      Delete seq -> Nothing
      New s -> Just <| PendingAmend r.sequence <| FormState (toString s.count) s.species s.location s.time
      Replace seq s -> Just <| PendingAmend r.sequence <| FormState (toString s.count) s.species s.location s.time

fromRecord : Maybe (Sequenced (Recording Sighting)) -> Maybe SightingForm
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
