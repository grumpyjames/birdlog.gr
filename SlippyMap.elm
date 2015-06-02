module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import MapBox exposing (mapBox)
import Movement exposing (keyState, mouseState)
import Osm exposing (openStreetMap)
import Tile exposing (render)
import TouchParser exposing (AffineComponents, Gesture(..), gestures)
import Tuple as T
import Types exposing (GeoPoint, Model, TileSource, Zoom)

import Color exposing (rgb)
import Graphics.Element exposing  (Element, centered, color, container, flow, layers, middle, right)
import Graphics.Input exposing (customButton, dropDown)
import Signal as S
import Text exposing (fromString)
import Time exposing (Time, fps)
import Window

defaultTileSrc = openStreetMap

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = 15.0
        initialModel = Model greenwich initialZoom (False, (0,0)) defaultTileSrc False
        draw = \window model -> layers [ render window model, buttons ]
    in S.map2 draw Window.dimensions (S.foldp applyEvent initialModel events)

-- Events
type Events = C Time | Z ZoomChange | M (Bool, (Int, Int)) | K (Int, Int) | T (Maybe TileSource) | G (Maybe Gesture)

-- FIXME: find a way to use fpsWhen
events : Signal Events
events = 
    let zooms = S.map Z <| zoomChange.signal 
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        mouse = S.map M mouseState
        tileSource = S.map T tileSrc.signal
        gests = S.map G gestures
        clockTicks = S.map C (fps 25) 
    in S.mergeMany [tileSource, clockTicks, zooms, gests, mouse, keys]


-- Applying events to the model
applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> (appIfClean applyZoom) m zo
 M mi -> (appIfClean applyMouse) m mi
 G ge -> (appIfClean applyGest) m ge
 K ke -> (appIfClean applyKeys) m ke
 C cl -> (appIfDirty applyTime) m cl 
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

appIfDirty : (Model -> a -> Model) -> (Model -> a -> Model)
appIfDirty f = \m a -> if (not m.dirty) then m else f m a

appIfClean : (Model -> a -> Model) -> (Model -> a -> Model)
appIfClean f = \m a -> if m.dirty then m else f m a 

-- Zoom controls and event
newZoom : ZoomChange -> Zoom -> Zoom
newZoom zc z = 
    case zc of
        In a -> z + a
        Out a -> z - a
        None -> z

type ZoomChange = In Float | Out Float | None

zoomChange = S.mailbox None

zoomIn = ourButton (S.message zoomChange.address (In 1)) "+"
zoomOut = ourButton (S.message zoomChange.address (Out 1)) "-"

-- FIXME: This doesn't necessarily converge!
applyTime : Model -> Time -> Model
applyTime m t = 
    let zoomAmount = 0.5 * (toFloat (round m.zoom) - m.zoom)
        (newZoom, done) = if ((abs zoomAmount) < 0.01) 
                          then (toFloat (round m.zoom), True)
                          else ((m.zoom + zoomAmount), False)
    in { m | zoom <- newZoom, dirty <- (not done) }

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = { m | zoom <- newZoom zc m.zoom }

-- Events that affect the centre point: drags and arrows
applyMouse : Model -> (Bool, (Int, Int)) -> Model
applyMouse model (isDown, (newX, newY)) = 
    case model.mouseState of
      (False, _) -> { model | mouseState <- (isDown, (newX, newY)) }
      (True, (oldX, oldY)) -> 
          let newModel = applyDrag model (oldX - newX, newY - oldY) 
          in { newModel | mouseState <- (isDown, (newX, newY)) }

applyKeys : Model -> (Int, Int) -> Model
applyKeys = applyDrag

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag = { m | centre <- move m.zoom m.centre drag } 

isInt : Zoom -> Bool
isInt z = (toFloat (round z)) == z

applyGest : Model -> Maybe Gesture -> Model
applyGest m g =
    let pct aff = T.combine (+) <| T.map (\ti -> ti / 2) aff.scale
    in case g of
      Just ge ->
          case ge of
            Drag (x, y) -> applyDrag m (-1 * x, y)
            Affine ac -> { m | zoom <- m.zoom * (pct ac) }
            End -> { m | dirty <- not (isInt m.zoom) } 
            otherwise -> m
      Nothing -> m             

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff = 
    let (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ (floor z)))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

-- Tile source : use a dropdown to switch between them
tileSrc : S.Mailbox (Maybe TileSource)
tileSrc = S.mailbox Nothing

accessToken = "pk.eyJ1IjoiZ3J1bXB5amFtZXMiLCJhIjoiNWQzZjdjMDY1YTI2MjExYTQ4ZWU4YjgwZGNmNjUzZmUifQ.BpRWJBEup08Z9DJzstigvg"

tileSrcDropDown : Element
tileSrcDropDown = 
    dropDown (S.message tileSrc.address)
             [ ("OpenStreetMap", Just openStreetMap)
             , ("ArcGIS", Just arcGIS)
             , ("MapBox", Just (mapBox "mapbox.run-bike-hike" accessToken))
             ]

-- user input 
buttons = flow right [zoomIn, zoomOut, tileSrcDropDown]

hoverC = rgb 240 240 240
downC = rgb 235 235 235
upC = rgb 248 248 248

ourButton : S.Message -> String -> Element
ourButton msg txt = 
    let el = centered <| fromString txt
        cn = container 30 30 middle el
        up = color upC cn
        down = color downC cn
        hover = color hoverC cn
    in customButton msg up hover down
