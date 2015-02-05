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
        globalOffset = globalPixelOffset m.tileSize tileCounts m.mapCenter
        tileOffsets = mapT (vid m.tileSize) m.mapCenter
        originTile = Tile <| tileOffsets `subtractT` (mapT (vid 2) tileCounts)
        tiles = cartesianProduct <| mergeT range originTile.coordinate tileCounts
        offsetFromTile = relativeTilePosition m.tileSize originTile
        draw = chain (drawTile renderer m.zoom m.tileSize) (doMove globalOffset offsetFromTile) 
     in layers <| [ (uncurry collage) m.window <| map (draw << Tile) tiles,
                    (uncurry spacer) m.window ]

globalPixelOffset : Int -> (Int, Int) -> (Int, Int) -> Position
globalPixelOffset tileSize tileCounts mapCenter =
    let pixelOffsets = (128, 128) `subtractT` (mapT (mer tileSize) mapCenter)
        originPixelOffsets = mapT (\a -> (a * tileSize) // -2) tileCounts
    in Position <| flipY <| addT originPixelOffsets pixelOffsets 

type alias F1 a = a -> a
type alias F2 a = a -> a -> a 

addP = lift2 addT
vid = flip (//)
mer = flip (%)

lift1 : (F1 (Int, Int)) -> (F1 Position)
lift1 g = \p -> Position <| g p.pixels

lift2 : (F2 (Int, Int)) -> (F2 Position)
lift2 g = \p1 p2 -> Position <| g p1.pixels p2.pixels

flipY = multiplyT (1, -1)

chain : (a -> b) -> (a -> b -> c) -> a -> c
chain f g = \a -> g a (f a)

relativeTilePosition : Int -> Tile -> Tile -> Position
relativeTilePosition tileSize originTile tile = 
    let relativeTile = tile.coordinate `subtractT` originTile.coordinate
        position = flipY <| mapT ((*) tileSize) <| relativeTile
    in Position position

doMove : Position -> (Tile -> Position) -> Tile -> Element -> Form
doMove globalOffset offsetter tile element =
    let tileSpecificOffset = offsetter tile
        distance = addP globalOffset tileSpecificOffset
    in move (mapT toFloat distance.pixels) <| toForm element
    
drawTile : TileRenderer -> Zoom -> Int -> Tile -> Element
drawTile r z tileSize t = r z tileSize t.coordinate

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

cartesianProduct : (List a, List b) -> List (a, b)
cartesianProduct (xs,ys) = 
    case xs of
      z :: zs -> (cartesianProduct (zs,ys)) ++ map ((,) z) ys
      [] -> []