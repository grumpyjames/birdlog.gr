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
render rdr tileSize zoom window c =
    let requiredTiles dim = (3 * tileSize + dim) // tileSize
        mapCenter = multiplyT (-1, 1) c 
        (xTiles, yTiles) = mapT requiredTiles window
        ((xTileOff, xPixelOff), (yTileOff, yPixelOff))  = mapT (toOffset tileSize) mapCenter
        (originX, originY) = subtractT (xTileOff, yTileOff) <| mapT ((//) 2) (xTiles, yTiles)
        basePosition = ((tileSize * xTiles) // (-2), (tileSize * yTiles) // 2)
        xRange = [originX..(originX + xTiles - 1)]
        yRange = [originY..(originY + yTiles - 1)]
        -- flip y's sign, elm treats co-ordinates sensible, OSM does not.
        pixelOffset = (128 - xPixelOff, yPixelOff - 128)
        (winX, winY) = window
        debugInfo = "originTile: " ++ toString (originX, originY) ++ ", centre: " ++ toString mapCenter
        grid = cartesianProduct xRange yRange
        tiles = L.map (step tileSize basePosition (originX, originY) pixelOffset) grid
        drawTiles renderer = collage winX winY <| L.map (ttf renderer zoom tileSize) <| tiles
     in layers <| [ drawTiles (wrap rdr),
                   drawTiles debug,
                   container winX winY middle <| plainText <| debugInfo,
                   spacer winX winY ]


step : Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Tile
step tileSize basePosition origin pixelOff coord = 
    let position = addT basePosition <| addT pixelOff <| flipY <| mapT ((*) tileSize) <| subtractT coord origin
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
