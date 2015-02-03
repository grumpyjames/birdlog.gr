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


type alias Tile = { point: (Int, Int), position: (Int, Int) }
type alias DrawnTile = { el: Element, positionOffset: (Int, Int) }

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
        tiles = map (makeTile m.tileSize originPixelOffsets originTileCoordinates pixelOffsets) <| (uncurry cartesianProduct) tileRanges
     in layers <| [ (uncurry collage) m.window <| map doMove <| map (drawTile renderer m.zoom m.tileSize) tiles,
                    (uncurry spacer) m.window ]

doMove : DrawnTile -> Form
doMove drawnTile = 
    let d = mapT toFloat drawnTile.positionOffset
    in move d <| toForm drawnTile.el

drawTile : TileRenderer -> Zoom -> Int -> Tile -> DrawnTile
drawTile r z tileSize t = DrawnTile (r z tileSize t.point) t.position

divAndRem : Int -> Int -> (Int, Int)
divAndRem divisor dividend = 
    let divides = dividend // divisor
        remainder = dividend % divisor
    in (divides, remainder)

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

makeTile : Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Tile
makeTile tileSize originPixelOffsets originCoordinates pixelOffsets tileCoordinates =
    let globalOffset = flipY <| addT originPixelOffsets pixelOffsets 
        position = addT globalOffset <| flipY <| mapT ((*) tileSize) <| tileCoordinates `subtractT` originCoordinates
    in Tile tileCoordinates position

-- There is no need for the TileRenderer to know anything other than the tile's coordinate
-- We provide 
type alias InnerRender = (Zoom -> Int -> Tile -> Element)

renderAndMove : TileRenderer -> Zoom -> Int -> Tile -> Form
renderAndMove render zoom tileSize t =
    let d = mapT toFloat t.position
    in move d <| toForm <| render zoom tileSize t.point

flipY : (Int, Int) -> (Int, Int)
flipY t = (fst t, (-1) * snd t)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ map ((,) z) ys
      [] -> []