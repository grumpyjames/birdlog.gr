module Tile (render) where

import Types exposing (Model, Position, Tile, TileUrl, Zoom(..))
import Functions exposing (chain)
import Graphics.Collage exposing (Form, collage, move, toForm)
import Graphics.Element exposing (Element, image, layers, spacer)
import List exposing (map)
import Tuple exposing (..)

render : (Int, Int) -> Model -> Element
render window m =
    let requiredTiles dim = (3 * m.tileSource.tileSize + dim) // m.tileSource.tileSize
        tileCounts = mapT requiredTiles window
        mapCentre = m.tileSource.locate m.zoom m.centre
        globalOffset = globalPixelOffset m.tileSource.tileSize tileCounts mapCentre.position
        origin = originTile mapCentre.tile tileCounts               
        tiles = cartesianProduct <| mergeT range origin.coordinate tileCounts
        draw = chain (tileEl m.tileSource.tileSize m.tileSource.tileUrl m.zoom) (doMove m.tileSource.tileSize origin globalOffset) 
     in layers <| [ 
                    (uncurry collage) window <| map (draw << Tile) tiles,
                    (uncurry spacer) window
                  ]


tileEl : Int -> TileUrl -> Zoom -> Tile -> Element
tileEl size url zoom tile = image size size <| url zoom tile

originTile : Tile -> (Int, Int) -> Tile
originTile centreTile tileCounts = Tile <| centreTile.coordinate `subtractT` (mapT (vid 2) tileCounts)

globalPixelOffset : Int -> (Int, Int) -> Position -> Position
globalPixelOffset tileSize tileCounts centrePixel =
    let pixelOffsets = (128, 128) `subtractT` centrePixel.pixels
        originPixelOffsets = mapT (\a -> tileSize * (a // -2)) tileCounts
    in Position <| flipY <| originPixelOffsets `addT` pixelOffsets 

type alias F2 a = a -> a -> a 

addP = lift2 addT
vid = flip (//)
mer = flip (%)

lift2 : (F2 (Int, Int)) -> (F2 Position)
lift2 g = \p1 p2 -> Position <| g p1.pixels p2.pixels

flipY = multiplyT (1, -1)

relativeTilePosition : Int -> Tile -> Tile -> Position
relativeTilePosition tileSize originTile tile = 
    let relativeTile = tile.coordinate `subtractT` originTile.coordinate
        position = flipY <| mapT ((*) tileSize) <| relativeTile
    in Position position

doMove : Int -> Tile -> Position -> Tile -> Element -> Form
doMove tileSize originTile globalOffset tile element =
    let tileSpecificOffset = relativeTilePosition tileSize originTile tile
        distance = addP globalOffset tileSpecificOffset
    in move (mapT toFloat distance.pixels) <| toForm element

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

cartesianProduct : (List a, List b) -> List (a, b)
cartesianProduct (xs,ys) = 
    case xs of
      z :: zs -> (cartesianProduct (zs,ys)) ++ map ((,) z) ys
      [] -> []