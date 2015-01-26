module Tile (Render, render) where

import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, middle, layers, spacer)
import List as L
import Text (plainText)
import Tuple (..)

type alias Tile = { point: (Int, Int), position: (Float, Float) }
type alias Render = Int -> (Int, Int) -> Element

render : Render -> Int -> (Int, Int) -> (Int, Int) -> Element
render rdr tileSize (winX, winY) moves = 
    let mapLayer = renderTileGrid tileSize winX winY moves
    in layers <| [ mapLayer (wrap rdr), mapLayer debug, spacer winX winY ]


type alias InnerRender = (Int -> Tile -> Element)

wrap : Render -> InnerRender
wrap f = \sz t -> f sz t.point 

renderTileGrid : Int -> Int -> Int -> (Int, Int) -> InnerRender -> Element
renderTileGrid size winX winY shift render = 
    let grid = coords 9 7
        tiles = L.map (step size (offset size shift)) grid
    in collage winX winY <| L.map (ttf render size) <| tiles

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

ttf : InnerRender -> Int -> Tile -> Form
ttf render size t = move t.position <| toForm <| render size t

debug : InnerRender
debug sz tile = container sz sz middle <| plainText <| toString tile.point ++ "\n" ++ toString tile.position

step : Int -> Tile -> (Int, Int) -> Tile
step size offsetTile baseCoordinate = 
    let basePosition = mapT (toFloat << ((*) size)) baseCoordinate
    in Tile (addT offsetTile.point baseCoordinate) (addT offsetTile.position basePosition) 

offset : Int -> (Int, Int) -> Tile
offset tileSize (x, y)  = 
    let wraps t = (0 - t) // tileSize
        pos t = toFloat <| (*) (sgn t) <| (abs t) % tileSize
    in Tile (wraps x, wraps y) (pos x, pos y)

coords : Int -> Int -> List (Int, Int)
coords xc yc = 
    let coordRange t = [(( 1 - t ) // 2)..((t - 1) // 2)]
    in cartesianProduct (coordRange xc) (coordRange yc)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ L.map ((,) z) ys
      [] -> []
