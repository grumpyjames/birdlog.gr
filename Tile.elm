module Tile (render) where

import Styles exposing (px, absolute, dimensions, position, zeroMargin)
import Model exposing (Model)
import Tuple as T
import Types exposing (GeoPoint, Position, Tile, TileSource, TileUrl, Zoom(..))

import Array exposing (Array, fromList, toList)
import Html exposing (Html, div, fromElement)
import Html.Attributes as Attr exposing (style)
import List exposing (map)

render : Model -> Html
render m = oneLayer m.centre m.windowSize (pickZoom m.zoom) m.tileSource

oneLayer : GeoPoint -> (Int, Int) -> Int -> TileSource -> Html 
oneLayer centre window zoom tileSource = 
    let tileSize = calcTileSize tileSource zoom
        requiredTiles dim = (3 * tileSource.tileSize + dim) // tileSource.tileSize
        tileCounts = T.map requiredTiles window
        mapCentre = tileSource.locate (toFloat zoom) centre
        originTile = origin mapCentre.tile tileCounts
        offset = originOffset window tileSize tileCounts mapCentre.position
        tileRows = rows (curry Tile) <| T.merge range originTile.coordinate tileCounts
        mapEl = flowTable tileSize (renderOneTile zoom tileSize tileSource.tileUrl) tileRows
        attr = style ([("overflow", "hidden")] ++ absolute ++ (dimensions window) ++ zeroMargin)
    in div [attr] [applyPosition mapEl offset]

pickZoom : Zoom -> Int
pickZoom zoom = 
    case zoom of
      Constant c -> c
      Between a b -> b

-- in the case of a fractional zoom, expand the tile size appropriately
calcTileSize : TileSource -> Int -> Int
calcTileSize tileSource zoom =
    let tileSize = tileSource.tileSize
        frac f = f - (toFloat (floor f))
        digizoom = floor ((frac (toFloat zoom)) * (toFloat tileSize))
    in tileSize + digizoom

applyPosition : Html -> Position -> Html
applyPosition el distance =
    let attr = style ((position (distance.pixels)) ++ absolute) 
    in div [attr] [el]

-- use the model to render a single tile
renderOneTile : Int -> Int -> TileUrl -> Tile -> Html
renderOneTile zoom tileSize url tile =
    Html.img ([Attr.src (url zoom tile), Attr.style (("display", "inline-block") :: (dimensions (tileSize, tileSize)))]) []
    
-- which tile should go in the top left hand corner?
origin : Tile -> (Int, Int) -> Tile
origin centreTile tileCounts = 
    Tile <| centreTile.coordinate `T.subtract` (T.map (vid 2) tileCounts)

-- How far should the origin tile's top left corner be from that of the viewport?
originOffset : (Int, Int) -> Int -> (Int, Int) -> Position -> Position
originOffset window tileSize tileCounts centrePixel =
    let totalTilingSize = T.map ((*) tileSize) tileCounts 
        centerInWindow = T.map (\pix -> pix // -2) (totalTilingSize `T.subtract` window)
        halfTile = tileSize // 2
        pixelOffsets = (halfTile, halfTile) `T.subtract` centrePixel.pixels
    in Position <| (centerInWindow `T.add` pixelOffsets)

-- Arrange an array of arrays in a nice table
flowTable : Int -> (a -> Html) -> List (List a) -> Html
flowTable tileSize renderer arr = 
    let row els = Html.div [style (zeroMargin ++ [("white-space", "nowrap"), ("height", px tileSize)])] (map renderer els)
    in div [] (map row arr)

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