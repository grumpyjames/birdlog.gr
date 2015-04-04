module SlippyMap (main) where

import ButtonDemo (ourButton)
import GeoPoint (GeoPoint)
import Movement (movement, deltas, keyState, mouseState)
import Osm (osm, tileSize, convert)

import Graphics.Element (flow, layers, right)
import Signal as S
import Tile (Model, Zoom(..), render)
import Tuple (..)
import Wheel (wheel)
import Window

main = 
    let gpt = GeoPoint 51.48 0.0
        initialZoom = Zoom 15
        initModel = Model tileSize gpt initialZoom convert (False, (0,0)) osm
        draw = \window model -> layers [ render window model, buttons ]
    in S.map2 draw Window.dimensions (S.foldp applyEvent initModel events)

applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 M mi -> applyMouse m mi
 K ke -> applyKeys m ke

buttons = flow right [zoomIn, zoomOut]

events : Signal Events
events = 
    let zooms = S.map Z <| S.subscribe zoomChange 
        keys = S.map K <| S.map (multiplyT (256, 256)) <| keyState
        mouse = S.map M mouseState
    in S.mergeMany [zooms, mouse, keys]

type Events = Z ZoomChange | M (Bool, (Int, Int)) | K (Int, Int)

move : Int -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff =
    let (dlon, dlat) = mapT (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ z))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = { m | zoom <- newZoom zc m.zoom }

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
applyDrag m drag =
    case m.zoom of
      Zoom z -> { m | centre <- move z m.centre drag } 

newZoom : ZoomChange -> Zoom -> Zoom
newZoom zc zoom = 
    case zoom of
      Zoom z -> case zc of
        In -> Zoom (z + 1)
        Out -> Zoom (z - 1)
        None -> Zoom z

type ZoomChange = In | Out | None

zoomChange = S.channel None

zoomIn = ourButton (S.send zoomChange In) "+"
zoomOut = ourButton (S.send zoomChange Out) "-"
