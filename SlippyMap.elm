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
 D d -> applyDrag m d
 Z z -> applyZoom m z
 W w -> applyWindow m w

buttons = flow right [zoomIn, zoomOut]

events : Signal Events
events = 
    let drags = S.map D <| S.map (multiplyT (-1, -1)) deltas
        zooms = S.map Z zoomChanges
        windows = S.map W Window.dimensions
    in S.mergeMany [windows, zooms, drags]

type Events = D (Int, Int) | Z ZoomChange | W (Int, Int)

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag =
    case m.zoom of
      Zoom z -> { m | centre <- move z m.centre drag } 

move : Int -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt (x, y) =
    let dlon = (toFloat x) * (1.0 / (toFloat (z * z))) * 0.1
        dlat = (toFloat y) * (1.0 / (toFloat (z * z))) * 0.1
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = { m | zoom <- newZoom zc m.zoom }

applyWindow : Model -> (Int, Int) -> Model
applyWindow m w = { m | window <- w }

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

zoomSignal =
    let zs = zoomChanges
        step zc z = 
            case zc of 
              In -> z + 1
              Out -> z - 1
              None -> z
    in S.map Zoom <| S.foldp step 9 zs