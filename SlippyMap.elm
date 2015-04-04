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

-- 'inverted' mouse, but elm's y and osms are opposite. Do any remaining flips below
main = 
    let gpt = GeoPoint 51.48 0.0
        initModel = Model tileSize gpt initialZoom (0,0) convert (False, (0,0))
        draw = \model -> layers [ render osm model, buttons ]
    in S.map draw <| S.foldp applyEvent initModel events

applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 W wi -> applyWindow m wi
 M mi -> applyMouse m mi
 K ke -> applyKeys m ke

buttons = flow right [zoomIn, zoomOut]

events : Signal Events
events = 
    let zooms = S.map Z zoomChanges
        windows = S.map W Window.dimensions
        keys = S.map K <| S.map (multiplyT (256, 256)) <| keyState
        mouse = S.map M mouseState
    in S.mergeMany [windows, mouse, zooms, keys]

type Events = Z ZoomChange | W (Int, Int) | M (Bool, (Int, Int)) | K (Int, Int)

move : Int -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt (x, y) =
    let dlon = (toFloat x) * (1.0 / (toFloat (z * z))) * 0.1
        dlat = (toFloat y) * (1.0 / (toFloat (z * z))) * 0.1
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = { m | zoom <- newZoom zc m.zoom }

applyWindow : Model -> (Int, Int) -> Model
applyWindow m w = { m | window <- w }

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

initialZoom = Zoom 9

zoomChanges = S.subscribe zoomChange 
