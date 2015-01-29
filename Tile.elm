module Tile (Render, Zoom(..), render) where

import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, middle, layers, spacer)
import List as L
import Text (plainText)
import Tuple (..)

type alias Tile = { point: (Int, Int), position: (Int, Int) }
type alias Render = Zoom -> Int -> (Int, Int) -> Element

type Zoom = Zoom Int

render : Render -> Int -> Zoom -> (Int, Int) -> (Int, Int) -> Element
render rdr tileSize zoom (winX, winY) moves = 
    let mapLayer = renderTileGrid tileSize zoom winX winY moves
    in layers <| [ mapLayer (wrap rdr), mapLayer debug, spacer winX winY ]


type alias InnerRender = (Zoom -> Int -> Tile -> Element)

wrap : Render -> InnerRender
wrap f = \z sz t -> f z sz t.point 

renderTileGrid : Int -> Zoom -> Int -> Int -> (Int, Int) -> InnerRender -> Element
renderTileGrid size zoom winX winY shift render = 
    let requiredTiles dim = (3 * size + dim) // size
        xTiles = requiredTiles winX
        yTiles = requiredTiles winY
        grid = coords xTiles yTiles
        tc c = (size * c) // 2
        posnOffset = (tc (-1 * xTiles), tc yTiles)
        tiles = L.map (step posnOffset size (offset size shift)) grid
    in collage winX winY <| L.map (ttf render (0,0) (0,0) zoom size) <| tiles

step : (Int, Int) -> Int -> Tile -> (Int, Int) -> Tile
step positionOffset size offset base = 
    let basePosition = addT positionOffset <| mapT ((*) size) <| flipY base
    in Tile (addT offset.point base) (addT offset.position basePosition) 

flipY : (Int, Int) -> (Int, Int)
flipY t = (fst t, (-1) * snd t)

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

applyCenter tileOff pixelOff tile = { point = addT tileOff tile.point, position = addT pixelOff tile.position }

ttf : InnerRender -> (Int, Int) -> (Int, Int) -> Zoom -> Int -> Tile -> Form
ttf render tileOff pixelOff zoom size t = move (mapT toFloat t.position) <| toForm <| render zoom size <| applyCenter tileOff pixelOff t

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
