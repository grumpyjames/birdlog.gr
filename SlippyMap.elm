module SlippyMap (main) where

import ArcGIS exposing (arcGIS)
import MapBox exposing (mapBox)
import Movement exposing (keyState, mouseState)
import Osm exposing (openStreetMap)
import Tile exposing (render)
import TouchParser exposing (Gesture(..), gestures)
import Tuple as T
import Types exposing (GeoPoint, Model, TileSource, Zoom)

import Color exposing (rgb)
import Graphics.Element exposing  (Element, centered, color, container, flow, layers, middle, right)
import Graphics.Input exposing (customButton, dropDown)
import Signal as S
import Text exposing (fromString)
import Window

defaultTileSrc = openStreetMap

main = 
    let greenwich = GeoPoint 51.48 0.0
        initialZoom = 15.0
        initialModel = Model greenwich initialZoom (False, (0,0)) defaultTileSrc
    in S.map2 view Window.dimensions (S.foldp applyEvent initialModel events)

view window model = layers (render window model ++ [(buttons zoomChange.address tileSrc.address)])

-- Mailboxes
zoomChange : S.Mailbox ZoomChange
zoomChange = S.mailbox (In 0)

tileSrc : S.Mailbox (Maybe TileSource)
tileSrc = S.mailbox Nothing

-- Events
type ZoomChange = In Float | Out Float

type Events = Z ZoomChange | M (Bool, (Int, Int)) | K (Int, Int) | T (Maybe TileSource) | G (Maybe Gesture)

events : Signal Events
events = 
    let zooms = S.map Z <| zoomChange.signal 
        keys = S.map K <| S.map (T.multiply (256, 256)) <| keyState
        mouse = S.map M mouseState
        tileSource = S.map T tileSrc.signal
        gests = S.map G gestures 
    in S.mergeMany [tileSource, zooms, gests, mouse, keys]


-- Applying events to the model
applyEvent : Events -> Model -> Model
applyEvent e m = case e of
 Z zo -> applyZoom m zo
 M mi -> applyMouse m mi
 G ge -> applyGest m ge
 K ke -> applyKeys m ke 
 T ti -> case ti of
           Just ts -> {m | tileSource <- ts }
           Nothing -> {m | tileSource <- defaultTileSrc }

-- Zoom controls and event
newZoom : ZoomChange-> Zoom -> Zoom
newZoom zc z = 
    case zc of
        In a -> z + a
        Out a -> z - a

zoomIn address = ourButton (S.message address (In 1)) "+"
zoomOut address = ourButton (S.message address (Out 1)) "-"

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
    case g of
      Just ge ->
          case ge of
            Drag (x, y) -> applyDrag m (-1 * x, y)
            otherwise -> m
      Nothing -> m             

move : Zoom -> GeoPoint -> (Int, Int) -> GeoPoint
move z gpt pixOff = 
    let (dlon, dlat) = T.map (\t -> (toFloat t) * 1.0 / (toFloat (2 ^ (floor z)))) pixOff
    in GeoPoint (gpt.lat + dlat) (gpt.lon + dlon)

accessToken = "pk.eyJ1IjoiZ3J1bXB5amFtZXMiLCJhIjoiNWQzZjdjMDY1YTI2MjExYTQ4ZWU4YjgwZGNmNjUzZmUifQ.BpRWJBEup08Z9DJzstigvg"

tileSrcDropDown : S.Address (Maybe TileSource) -> Element
tileSrcDropDown address = 
    dropDown (S.message address)
             [ ("OpenStreetMap", Just openStreetMap)
             , ("ArcGIS", Just arcGIS)
             , ("MapBox", Just (mapBox "mapbox.run-bike-hike" accessToken))
             ]

-- user input 
buttons zoomAddress tileSrcAddress = flow right [zoomIn zoomAddress, zoomOut zoomAddress, tileSrcDropDown tileSrcAddress]

hoverC = rgb 240 240 240
downC = rgb 235 235 235
upC = rgb 248 248 248

ourButton : S.Message -> String -> Element
ourButton msg txt = 
    let el = centered <| fromString txt
        cn = container 100 100 middle el
        up = color upC cn
        down = color downC cn
        hover = color hoverC cn
    in customButton msg up hover down
