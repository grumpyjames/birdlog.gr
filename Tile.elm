module Tile (Render, Model, Zoom(..), render) where

import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, middle, layers, spacer)
import List as L
import Text (plainText)
import Tuple (..)

type alias Render = Zoom -> Int -> (Int, Int) -> Element
type Zoom = Zoom Int

type alias Model = {
      tileSize : Int,
      zoom : Zoom,
      window : (Int, Int),
      mapCenter : (Int, Int)
}

render : Render -> Model -> Element
render rdr m =
    let requiredTiles dim = (3 * m.tileSize + dim) // m.tileSize
        tileCounts = mapT requiredTiles m.window
        offsets = mapT (divAndRem m.tileSize) m.mapCenter
        pixelOffsets = (128, 128) `subtractT` (mapT snd offsets)
        tileOffsets = mapT fst offsets
        vid = flip (//)
        originTileCoordinates = tileOffsets `subtractT` (mapT (vid 2) tileCounts)
        originPixelOffsets = mapT (vid -2) <| mapT ((*) m.tileSize) tileCounts
        tileRanges = mergeT tileRange originTileCoordinates tileCounts
        tiles = L.map (makeTile m.tileSize originPixelOffsets originTileCoordinates pixelOffsets) <| (uncurry cartesianProduct) tileRanges
        drawTiles renderer = (uncurry collage) m.window <| L.map (ttf renderer m.zoom m.tileSize) <| tiles
     in layers <| [ drawTiles (wrap rdr), (uncurry spacer) m.window ]

divAndRem : Int -> Int -> (Int, Int)
divAndRem divisor dividend = 
    let divides = dividend // divisor
        remainder = dividend % divisor
    in (divides, remainder)

tileRange : Int -> Int -> List Int
tileRange origin count = [origin..(origin + count - 1)]

type alias Tile = { point: (Int, Int), position: (Int, Int) }

makeTile : Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Tile
makeTile tileSize originPixelOffsets originCoordinates pixelOffsets tileCoordinates =
    let globalOffset = flipY <| addT originPixelOffsets pixelOffsets 
        position = addT globalOffset <| flipY <| mapT ((*) tileSize) <| tileCoordinates `subtractT` originCoordinates
    in Tile tileCoordinates position

type alias InnerRender = (Zoom -> Int -> Tile -> Element)

wrap : Render -> InnerRender
wrap f = \z sz t -> f z sz t.point 

ttf : InnerRender -> Zoom -> Int -> Tile -> Form
ttf render zoom tileSize t =
    let d = mapT toFloat t.position
    in move d <| toForm <| render zoom tileSize t

flipY : (Int, Int) -> (Int, Int)
flipY t = (fst t, (-1) * snd t)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ L.map ((,) z) ys
      [] -> []