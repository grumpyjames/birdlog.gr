module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import Movement exposing (keyState, mouseState)
import Osm exposing (openStreetMap)
import Tile exposing (render)
import Tuple as T
import Types exposing (GeoPoint, Zoom(..), Model, TileSource)

import Color exposing (rgb)
import Graphics.Element exposing  (Element, centered, color, container, flow, layers, middle, right)
import Graphics.Input exposing (customButton, dropDown)
import Signal as S
import Text exposing (fromString)
import Window

defaultTileSrc = openStreetMap

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = Zoom 15
        initialModel = Model greenwich initialZoom (False, (0,0)) defaultTileSrc
        draw = \window model -> layers [ render window model, buttons ]
    in S.map2 draw Window.dimensions (S.foldp applyEvent initialModel events)

-- Events
type Events = Z ZoomChange | M (Bool, (Int, Int)) | K (Int, Int) | T (Maybe TileSource)

events : Signal Events
events = 
    let zooms = S.map Z <| zoomChange.signal 
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        mouse = S.map M mouseState
        tileSource = S.map T tileSrc.signal
    in S.mergeMany [tileSource, zooms, mouse, keys]


-- Applying events to the model
applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 M mi -> applyMouse m mi
 K ke -> applyKeys m ke
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

-- Zoom controls and event
newZoom : ZoomChange -> Zoom -> Zoom
newZoom zc zoom = 
    case zoom of
      Zoom z -> case zc of
        In -> Zoom (z + 1)
        Out -> Zoom (z - 1)
        None -> Zoom z

type ZoomChange = In | Out | None

zoomChange = S.mailbox None

zoomIn = ourButton (S.message zoomChange.address In) "+"
zoomOut = ourButton (S.message zoomChange.address Out) "-"

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

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move zoom gpt pixOff = case zoom of
    Zoom z -> let (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ z))) pixOff
              in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

-- Tile source : use a dropdown to switch between them
tileSrc : S.Mailbox (Maybe TileSource)
tileSrc = S.mailbox Nothing

tileSrcDropDown : Element
tileSrcDropDown = 
    dropDown (S.message tileSrc.address)
             [ ("OpenStreetMap", Just openStreetMap)
             , ("ArcGIS", Just arcGIS)
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
