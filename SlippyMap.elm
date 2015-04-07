module SlippyMap (main) where

import ButtonDemo (ourButton)
import Types (GeoPoint, Zoom(..), Model, TileSource)
import Movement (movement, deltas, keyState, mouseState)
import Osm (openStreetMap)
import ArcGIS (arcGIS)

import Graphics.Element (Element, flow, layers, right)
import Graphics.Input (dropDown)
import Signal as S
import Tile (render)
import Tuple (..)
import Wheel (wheel)
import Window

defaultTileSrc = openStreetMap

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = Zoom 15
        initModel = Model greenwich initialZoom (False, (0,0)) defaultTileSrc
        draw = \window model -> layers [ render window model, buttons ]
    in S.map2 draw Window.dimensions (S.foldp applyEvent initModel events)

tileSrc : S.Channel (Maybe TileSource)
tileSrc = S.channel Nothing

tileSrcDropDown : Element
tileSrcDropDown = 
    dropDown (S.send tileSrc)
             [ ("OpenStreetMap", Just openStreetMap)
             , ("ArcGIS", Just arcGIS)
             ]

applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 M mi -> applyMouse m mi
 K ke -> applyKeys m ke
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

buttons = flow right [zoomIn, zoomOut, tileSrcDropDown]

events : Signal Events
events = 
    let zooms = S.map Z <| S.subscribe zoomChange 
        keys = S.map K <| S.map (multiplyT (256, 256)) <| keyState
        mouse = S.map M mouseState
        tileSource = S.map T <| S.subscribe tileSrc
    in S.mergeMany [tileSource, zooms, mouse, keys]

type Events = Z ZoomChange | M (Bool, (Int, Int)) | K (Int, Int) | T (Maybe TileSource)

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
