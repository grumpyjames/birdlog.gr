module SlippyMap (main) where

import ButtonDemo (ourButton)
import GeoPoint (GeoPoint)
import Movement (movement, deltas)
import Osm (osm, tileSize, convert)

import Graphics.Element (flow, layers, right)
import Signal as S
import Tile (Model, Zoom(..), render)
import Tuple (..)
import Wheel (wheel)
import Window

-- 'inverted' mouse, but elm's y and osms are opposite. Do any remaining flips below
main = 
    let initialCenter = mapT ((*) tileSize) (16, 7)
        mapCenter = S.map (addT initialCenter << multiplyT (-1, 1)) movement
        zoom = zoomSignal
        gpt = GeoPoint 51.48 0.0
        draw = \model -> layers [ render osm model, buttons ]
    in S.map draw <| S.map3 (Model tileSize gpt) zoom Window.dimensions (S.constant convert)

buttons = flow right [zoomIn, zoomOut]

events : Signal Events
events = 
    let drags = S.map D deltas
        zooms = S.map Z zoomChanges
        windows = S.map W Window.dimensions
    in S.mergeMany [drags, zooms, windows]

type Events = D (Int, Int) | Z ZoomChange | W (Int, Int)

applyDrag : Model -> (Int, Int) -> Model
applyDrag m drag =
    case m.zoom of
      Zoom z -> { m | centre <- move z m.centre drag } 

move : Int -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt (x, y) =
    let dx = toFloat (x * z)
        dy = toFloat (y * z)
    in GeoPoint (gpt.lat + dx) (gpt.lon + dy)

applyZoom : Model -> ZoomChange -> Model
applyZoom m zc = { m | zoom <- newZoom zc m.zoom }

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

initialZoom = 9

zoomChanges = S.subscribe zoomChange 

zoomSignal =
    let zs = zoomChanges
        step zc z = 
            case zc of 
              In -> z + 1
              Out -> z - 1
              None -> z
    in S.map Zoom <| S.foldp step initialZoom zs