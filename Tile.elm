module Tile (render, Ready) where

import Styles exposing (px, absolute, dimensions, position, zeroMargin)
import Model exposing (Model)
import Tuple as T
import Types exposing (GeoPoint, Position, Tile, TileSource, TileUrl, Zoom(..))

import Html exposing (Attribute, Html, div, fromElement)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on)
import Json.Decode as J
import List exposing (map)
import Signal as S

type alias Ready = Int

render : S.Address Ready -> GeoPoint -> (Int, Int) -> Zoom -> TileSource -> Html
render addr centre window zoom tileSource =
    let wrapper content = 
            div [style ([("overflow", "hidden")] ++ absolute ++ (dimensions window) ++ zeroMargin)] content
        onLoad zoomLayer = on "load" (J.succeed zoomLayer) (\z -> S.message addr zoomLayer)
        noDisplay = style [("display", "none")]
    in case zoom of 
      Constant c -> wrapper <| [oneLayer [] centre tileSource window 0 c]
      (Between x1 x2 progress) -> wrapper <| [ oneLayer [] centre tileSource window (x1 - x2) x1
                                             , oneLayer [onLoad x2, noDisplay] centre tileSource window 0 x2]
                    
oneLayer : List Attribute -> GeoPoint -> TileSource -> (Int, Int) -> Int -> Int -> Html 
oneLayer attrs centre tileSource window dz zoom =
    let requiredTiles dim = (3 * tileSource.tileSize + dim) // tileSource.tileSize
        tileCounts = T.map requiredTiles window           
        mapCentre = tileSource.locate (toFloat zoom) centre
        -- scale things based on the differential zoom provided
        scl = scale dz
        zoomWindow = T.map (scl) window
        zoomTileSize = scl tileSource.tileSize
        -- work out how far the desired centre is from the top left of the tiles we will render
        originTile = origin mapCentre.tile tileCounts
        centreTileOffset = mapCentre.tile.coordinate `T.subtract` originTile.coordinate
        relativeCentre = (T.map scl mapCentre.position.pixels) `T.add` (T.map ((*) zoomTileSize) centreTileOffset) 
        -- work out how far to shift the map div to make the desired centre the centre
        offset = originOffset window relativeCentre
        tileRows = rows (curry Tile) <| T.merge range originTile.coordinate tileCounts
        mapEl = flowTable zoomTileSize (renderOneTile attrs zoom zoomTileSize tileSource.tileUrl) tileRows
    in applyPosition mapEl offset

scale : Int -> Int -> Int
scale dz a = if dz > 0 then a // (2 ^ dz) else a * (2 ^ (-1 * dz))

applyPosition : Html -> (Int, Int) -> Html
applyPosition el pixels =
    let attr = style ((position pixels) ++ absolute) 
    in div [attr] [el]

-- use the model to render a single tile
renderOneTile : List Attribute -> Int -> Int -> TileUrl -> Tile -> Html
renderOneTile attrs zoom tileSize url tile =
    Html.img (attrs ++ [Attr.src (url zoom tile), Attr.style (("display", "inline-block") :: (dimensions (tileSize, tileSize)))]) []
    
-- which tile should go in the top left hand corner?
origin : Tile -> (Int, Int) -> Tile
origin centreTile tileCounts = 
    Tile <| centreTile.coordinate `T.subtract` (T.map (vid 2) tileCounts)

-- How far should the origin tile's top left corner be from that of the viewport,
-- given that the centre has this pixel offset in the render area
originOffset : (Int, Int) -> (Int, Int) -> (Int, Int)
originOffset window centreOffset =
    let halfWindow = T.map (\pix -> pix // 2) window
    in halfWindow `T.subtract` centreOffset

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