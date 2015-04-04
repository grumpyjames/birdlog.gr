module Tile (Model, TileRenderer, Zoom(..), render) where

import GeoPoint (GeoPoint, TileOffset)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, layers, spacer)
import List (map)
import Text (plainText)
import Tuple (..)

type alias TileRenderer = Zoom -> Int -> (Int, Int) -> Element
type Zoom = Zoom Int

type alias Model = {
      tileSize : Int,
      centre : GeoPoint,
      zoom : Zoom,
      converter : Zoom -> GeoPoint -> (TileOffset, TileOffset),
      mouseState : (Bool, (Int, Int))
}

type alias Tile = { coordinate : (Int, Int) }
type alias Position = { pixels : (Int, Int) }

render : TileRenderer -> (Int, Int) -> Model -> Element
render renderer window m =
    let requiredTiles dim = (3 * m.tileSize + dim) // m.tileSize
        tileCounts = mapT requiredTiles window
        mapCentre = m.converter m.zoom m.centre
        centreTile = mapT (\off -> off.index) mapCentre
        centrePixel = mapT (\off -> off.pixel) mapCentre
        globalOffset = Position <| globalPixelOffset m.tileSize tileCounts centrePixel
        origin = Tile <| originTile centreTile tileCounts               
        tiles = cartesianProduct <| mergeT range origin.coordinate tileCounts
        draw = chain (drawTile renderer m.zoom m.tileSize) (doMove m.tileSize origin globalOffset) 
     in layers <| [ 
                    (uncurry collage) window <| map (draw << Tile) tiles,
                    (uncurry spacer) window,
                    plainText <| "\n\ncentre: " ++ (toString centreTile) ++ ", tileCounts: " ++ (toString tileCounts) ++ ", origin: " ++ (toString origin) ++ ", window:" ++ (toString window) ++ ", globalOffset: " ++ (toString globalOffset) ++ ", centrePixel: " ++ (toString centrePixel) ++ ", coord: " ++ (toString m.centre) 
                  ]

originTile : (Int, Int) -> (Int, Int) -> (Int, Int)
originTile centreTile tileCounts = centreTile `subtractT` (mapT (vid 2) tileCounts)

globalPixelOffset : Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
globalPixelOffset tileSize tileCounts centrePixel =
    let pixelOffsets = subtractT (128, 128) centrePixel
        originPixelOffsets = mapT (\a -> tileSize * (a // -2)) tileCounts
    in flipY <| addT originPixelOffsets pixelOffsets 

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

doMove : Int -> Tile -> Position -> Tile -> Element -> Form
doMove tileSize originTile globalOffset tile element =
    let tileSpecificOffset = relativeTilePosition tileSize originTile tile
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