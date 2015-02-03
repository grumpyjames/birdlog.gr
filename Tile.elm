module Tile (Model, TileRenderer, Zoom(..), render) where

import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, layers, spacer)
import List (map)
import Tuple (..)

type alias TileRenderer = Zoom -> Int -> (Int, Int) -> Element
type Zoom = Zoom Int

type alias Model = {
      tileSize : Int,
      zoom : Zoom,
      window : (Int, Int),
      mapCenter : (Int, Int)
}


type alias Tile = { coordinate : (Int, Int) }
type alias Position = { pixels : (Int, Int) }
type alias DrawnTile = { el: Element, tile: Tile }

render : TileRenderer -> Model -> Element
render renderer m =
    let requiredTiles dim = (3 * m.tileSize + dim) // m.tileSize
        tileCounts = mapT requiredTiles m.window
        offsets = mapT (divAndRem m.tileSize) m.mapCenter
        pixelOffsets = (128, 128) `subtractT` (mapT snd offsets)
        tileOffsets = mapT fst offsets
        vid = flip (//)
        originTileCoordinates = tileOffsets `subtractT` (mapT (vid 2) tileCounts)
        originPixelOffsets = mapT (vid -2) <| mapT ((*) m.tileSize) tileCounts
        tileRanges = mergeT range originTileCoordinates tileCounts
        globalOffset = flipY <| addT originPixelOffsets pixelOffsets 
        tiles = map Tile <| (uncurry cartesianProduct) tileRanges
        offsetFromTile = relativeTilePosition m.tileSize (Tile originTileCoordinates)
     in layers <| [ (uncurry collage) m.window <| map (doMove globalOffset offsetFromTile) <| map (drawTile renderer m.zoom m.tileSize) tiles,
                    (uncurry spacer) m.window ]

relativeTilePosition : Int -> Tile -> Tile -> Position
relativeTilePosition tileSize originTile tile = 
    let relativeTile = tile.coordinate `subtractT` originTile.coordinate
        position = flipY <| mapT ((*) tileSize) <| relativeTile
    in Position position

doMove : (Int, Int) -> (Tile -> Position) -> DrawnTile -> Form
doMove globalOffset offsetter drawnTile = 
    let tileSpecificOffset = offsetter drawnTile.tile
        d = mapT toFloat <| addT globalOffset tileSpecificOffset.pixels
    in move d <| toForm drawnTile.el

drawTile : TileRenderer -> Zoom -> Int -> Tile -> DrawnTile
drawTile r z tileSize t = DrawnTile (r z tileSize t.coordinate) t

divAndRem : Int -> Int -> (Int, Int)
divAndRem divisor dividend = 
    let divides = dividend // divisor
        remainder = dividend % divisor
    in (divides, remainder)

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

flipY : (Int, Int) -> (Int, Int)
flipY t = (fst t, (-1) * snd t)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ map ((,) z) ys
      [] -> []