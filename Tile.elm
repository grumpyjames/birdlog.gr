module Tile (render) where

import Tuple as T
import Types exposing (Model, Position, Tile, TileUrl, Zoom)

import Array exposing (Array, fromList, toList)
import Graphics.Collage exposing (Form, collage, move, toForm)
import Graphics.Element exposing (Element, down, flow, image, layers, right, spacer)
import List exposing (concatMap, map)

render : (Int, Int) -> Model -> Element
render window m =
    let tileSize = calcTileSize m
        requiredTiles dim = (3 * m.tileSource.tileSize + dim) // m.tileSource.tileSize
        tileCounts = T.map requiredTiles window
        mapCentre = m.tileSource.locate m.zoom m.centre
        originTile = origin mapCentre.tile tileCounts
        offset = originOffset tileSize tileCounts mapCentre.position
        tileRows = rows (curry Tile) <| T.merge range originTile.coordinate tileCounts
        mapEl = flowTable (renderOneTile m.zoom tileSize m.tileSource.tileUrl) tileRows
     in layers [ 
                  (uncurry collage) window [applyPosition mapEl offset],
                  (uncurry spacer) window
               ]        

calcTileSize : Model -> Int
calcTileSize m =
    let frac f = f - (toFloat (floor f))
        digizoom = floor ((frac m.zoom) * 256)
    in m.tileSource.tileSize + digizoom

applyPosition : Element -> Position -> Form
applyPosition el distance = move (T.map toFloat distance.pixels) <| toForm el

-- use the model to render a single tile
renderOneTile : Zoom -> Int -> TileUrl -> Tile -> Element
renderOneTile zoom tileSize url tile = 
    image tileSize tileSize <| url zoom tile

-- which tile should go in the top left hand corner?
origin : Tile -> (Int, Int) -> Tile
origin centreTile tileCounts = 
    Tile <| centreTile.coordinate `T.subtract` (T.map (vid 2) tileCounts)

-- How far should the origin tile's top left corner be from that of the viewport?
originOffset : Int -> (Int, Int) -> Position -> Position
originOffset tileSize tileCounts centrePixel =
    let halfTile = tileSize // 2
        pixelOffsets = (halfTile, halfTile) `T.subtract` centrePixel.pixels
    in Position <| flipY pixelOffsets

-- Arrange an array of arrays in a nice table
flowTable : (a -> Element) -> List (List a) -> Element
flowTable renderer arr = 
    let flowRender dir r els = flow dir <| map r els
    in flowRender down (flowRender right renderer) arr

-- supporting functions
type alias F2 a = a -> a -> a 

addP = lift2 T.add
vid = flip (//)
mer = flip (%)

lift2 : (F2 (Int, Int)) -> (F2 Position)
lift2 g = \p1 p2 -> Position <| g p1.pixels p2.pixels

flipY = T.multiply (1, -1)

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

rows : (a -> b -> c) -> (List a, List b) -> List (List c)
rows f (xs, ys) = map (\y -> map (\x -> (f x y)) xs) ys