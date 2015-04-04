module SlippyMap (main) where

import ButtonDemo (ourButton)
import GeoPoint (GeoPoint)
import Movement (movement)
import Osm (osm, tileSize)

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
    in S.map draw <| S.map3 (Model tileSize gpt) zoom Window.dimensions mapCenter

buttons = flow right [zoomIn, zoomOut]

type ZoomChange = In | Out | None

zoomChange = S.channel None

zoomIn = ourButton (S.send zoomChange In) "+"
zoomOut = ourButton (S.send zoomChange Out) "-"

initialZoom = 5

zoomSignal =
    let zs = S.subscribe zoomChange
        step zc z = 
            case zc of 
              In -> z + 1
              Out -> z - 1
              None -> z
    in S.map Zoom <| S.foldp step initialZoom zs