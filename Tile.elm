module Tile where

import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, image, middle, layers, spacer)
import List as L
import Movement (movement)
import Signal as S
import Text (plainText)
import Tuple (..)
import Window

type alias Tile = { point: (Int, Int), position: (Float, Float) }

main : Signal Element
main = S.map2 render Window.dimensions movement

render : (Int, Int) -> (Int, Int) -> Element
render (winX, winY) moves = 
    let mapLayer = renderTileGrid winX winY moves
    in layers <| [ mapLayer osm, mapLayer debug, spacer winX winY ] 

renderTileGrid : Int -> Int -> (Int, Int) -> Render -> Element
renderTileGrid winX winY shift render = 
    let size = 256
        grid = coords 9 7
        tiles = L.map (step size (offset size shift)) grid
    in collage winX winY <| L.map (ttf render size) <| tiles

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

type alias Render = (Int -> Tile -> Element)

ttf : Render -> Int -> Tile -> Form
ttf render size t = move t.position <| toForm <| render size t

osm : Render
osm sz tile = osmTile sz tile.point

debug : Render
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

-- osm specifics
log = logBase e
tiley2lat y z = 
    let n = pi - 2.0 * pi * y / (2.0 ^ z)
    in 180.0 / pi * atan ( 0.5 * ( (e ^ n) - (e ^ (-n) ) ) )                
long2tilex lon z = floor((lon + 180.0) / 360.0 * (2.0 ^ z)) 
lat2tiley lat z = floor((1.0 - log( tan(lat * pi/180.0) + 1.0 / cos(lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ z))
tilex2long x z = x / (2.0 ^ z) * 360.0 - 180

osmUrl : Int -> (Int, Int) -> String
osmUrl zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString (x+10)) ++ "/" ++ (toString ((1-y)+10)) ++ ".png"

osmTile : Int -> (Int, Int) -> Element
osmTile size point = image size size <| osmUrl 5 point