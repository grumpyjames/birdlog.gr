module Tile (Render, Model, Zoom(..), render) where

import Color (grey)
import Graphics.Collage (Form, collage, group, move, toForm)
import Graphics.Element (Element, color, container, middle, layers, spacer)
import List as L
import Text (plainText)
import Tuple (..)

type alias Tile = { point: (Int, Int), position: (Int, Int) }
type alias Render = Zoom -> Int -> (Int, Int) -> Element
type Zoom = Zoom Int

type alias Model = {
      zoom : Zoom,
      window : (Int, Int),
      mapCenter : (Int, Int)
}

render : Render -> Int -> Model -> Element
render rdr tileSize model =
    let requiredTiles dim = (3 * tileSize + dim) // tileSize
        tileCounts = mapT requiredTiles model.window
        ((xTileOff, xPixelOff), (yTileOff, yPixelOff)) = mapT (toOffset tileSize) model.mapCenter
        origin = subtractT (xTileOff, yTileOff) <| mapT (\a -> a // 2) tileCounts
        basePosition = mapT (\a -> a // (-2)) <| mapT ((*) tileSize) tileCounts
        pixelOffset = (128 - xPixelOff, 128 - yPixelOff)
        (winX, winY) = model.window
        debugInfo = "basePosition: " ++ toString basePosition ++ ", originTile: " ++ toString origin ++ ", centre: " ++ toString model.mapCenter
        tiles = L.map (step tileSize origin) <| tileRange origin tileCounts
        drawTiles renderer = collage winX winY <| groupAndOffset basePosition pixelOffset <| L.map (ttf renderer model.zoom tileSize) <| tiles
     in layers <| [ drawTiles (wrap rdr),
                   drawTiles debug,
                   container winX winY middle <| plainText <| debugInfo,
                   spacer winX winY ]

groupAndOffset : (Int, Int) -> (Int, Int) -> List Form -> List Form
groupAndOffset basePosition pixelOff forms = 
    let uniform = group forms
        offset = mapT toFloat <| flipY <| addT basePosition pixelOff
    in [ move offset uniform ]  

tileRange : (Int, Int) -> (Int, Int) -> List (Int, Int)
tileRange (originX, originY) (countX, countY) =
    let range start size = [start..(start + size - 1)]
    in cartesianProduct (range originX countX) (range originY countY)

step : Int -> (Int, Int) -> (Int, Int) -> Tile
step tileSize origin coord = 
    let position = flipY <| mapT ((*) tileSize) <| subtractT coord origin
    in Tile coord position

toOffset : Int -> Int -> (Int, Int)
toOffset tileSize coord = 
    let index = coord // tileSize
        pixel = coord - (index * tileSize)
    in ( index, pixel )


type alias InnerRender = (Zoom -> Int -> Tile -> Element)

wrap : Render -> InnerRender
wrap f = \z sz t -> f z sz t.point 


ttf : InnerRender -> Zoom -> Int -> Tile -> Form
ttf render zoom tileSize t =
    let d = mapT toFloat t.position
    in move d <| toForm <| render zoom tileSize t

flipY : (Int, Int) -> (Int, Int)
flipY t = (fst t, (-1) * snd t)

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

applyCenter tileOff pixelOff tile = { point = addT tileOff tile.point, position = addT pixelOff tile.position }

debug : InnerRender
debug zoom sz tile = container sz sz middle <| plainText <| toString tile.point ++ "\n" ++ toString tile.position

-- given a tile size, and an amount of movement in pixels, give a tile offset by which to move everything
-- wraps give the number of tiles that should wrap, the remainder gives the actual pixel movement.
offset : Int -> (Int, Int) -> Tile
offset tileSize (x, y)  = 
    let wraps t = (0 - t) // tileSize
        pos t = (*) (sgn t) <| (abs t) % tileSize
    in Tile (wraps x, wraps (-y)) (pos x, pos y)

coords : Int -> Int -> List (Int, Int)
coords xc yc = 
    let coordRange t = [0..t]
    in cartesianProduct (coordRange xc) (coordRange yc)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ L.map ((,) z) ys
      [] -> []
