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

render : TileRenderer -> Model -> Element
render renderer m =
    let requiredTiles dim = (3 * m.tileSize + dim) // m.tileSize
        tileCounts = mapT requiredTiles m.window
        offsets = mapT (divAndRem m.tileSize) m.mapCenter
        pixelOffsets = (128, 128) `subtractT` (mapT snd offsets)
        tileOffsets = mapT fst offsets
        vid = flip (//)
        originTile = Tile <| tileOffsets `subtractT` (mapT (vid 2) tileCounts)
        originPixelOffsets = mapT (vid -2) <| mapT ((*) m.tileSize) tileCounts
        tileRanges = mergeT range originTile.coordinate tileCounts
        globalOffset = flipY <| addT originPixelOffsets pixelOffsets 
        tiles = map Tile <| (uncurry cartesianProduct) tileRanges
        offsetFromTile = relativeTilePosition m.tileSize originTile
        draw = chain (drawTile renderer m.zoom m.tileSize) (doMove globalOffset offsetFromTile) 
     in layers <| [ (uncurry collage) m.window <| map draw tiles,
                    (uncurry spacer) m.window ]

chain : (a -> b) -> (a -> b -> c) -> a -> c
chain f g = \a -> g a (f a)

relativeTilePosition : Int -> Tile -> Tile -> Position
relativeTilePosition tileSize originTile tile = 
    let relativeTile = tile.coordinate `subtractT` originTile.coordinate
        position = flipY <| mapT ((*) tileSize) <| relativeTile
    in Position position

doMove : (Int, Int) -> (Tile -> Position) -> Tile -> Element -> Form
doMove globalOffset offsetter tile element =
    let tileSpecificOffset = offsetter tile
        distance = mapT toFloat <| addT globalOffset tileSpecificOffset.pixels
    in move distance <| toForm element
    
drawTile : TileRenderer -> Zoom -> Int -> Tile -> Element
drawTile r z tileSize t = r z tileSize t.coordinate

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