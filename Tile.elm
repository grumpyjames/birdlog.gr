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
      zoom : Zoom,
      window : (Int, Int),
      mapCenter : (Int, Int)
}

render : Render -> Int -> Model -> Element
render rdr tileSize model =
    let requiredTiles dim = (3 * tileSize + dim) // tileSize
        tileCounts = mapT requiredTiles model.window
        offsets = mapT (divAndRem tileSize) model.mapCenter
        pixelOffsets = (128, 128) `subtractT` (mapT snd offsets)
        tileOffsets = mapT fst offsets
        vid a b = b // a
        originTileCoordinates = tileOffsets `subtractT` (mapT (vid 2) tileCounts)
        basePosition = mapT (vid -2) <| mapT ((*) tileSize) tileCounts
        tileRanges = mergeT tileRange originTileCoordinates tileCounts
        (winX, winY) = model.window
        tiles = L.map (makeTile tileSize basePosition originTileCoordinates pixelOffsets) <| (uncurry cartesianProduct) tileRanges
        drawTiles renderer = collage winX winY <| L.map (ttf renderer model.zoom tileSize) <| tiles
     in layers <| [ drawTiles (wrap rdr),
                    spacer winX winY ]

divAndRem : Int -> Int -> (Int, Int)
divAndRem divisor dividend = 
    let divides = dividend // divisor
        remainder = dividend % divisor
    in (divides, remainder)

tileRange : Int -> Int -> List Int
tileRange origin count = [origin..(origin + count - 1)]

type alias Tile = { point: (Int, Int), position: (Int, Int) }

makeTile : Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Tile
makeTile tileSize originOffsets originCoordinates pixelOffsets tileCoordinates =
    let globalOffset = flipY <| addT originOffsets pixelOffsets 
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