module Tile ( render
            , Progress) where

import Styles exposing (px, absolute, dimensions, noDisplay, position, zeroMargin)
import Model exposing (Model)
import Tuple as T
import Types exposing (GeoPoint, MapState, Position, Tile, TileSource, TileUrl, Zoom(..))

import Html exposing (Attribute, Html, div, fromElement)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (on)
import Json.Decode as J
import List exposing (map)
import Maybe as M
import Signal as S

-- zoom, progressIncrement
type alias Progress = (Int, Float)

render : S.Address Progress -> (MapState a) -> Html
render addr map =
    let wrapper content = 
            div [style ([("overflow", "hidden")] ++ absolute ++ (dimensions map.windowSize) ++ zeroMargin)] content
    in case map.zoom of 
      Constant c -> wrapper <| [oneLayer Nothing map.centre map.tileSource map.windowSize 0 c]
      (Between x1 x2 progress) -> wrapper <| [ oneLayer Nothing map.centre map.tileSource map.windowSize (x1 - x2) x1
                                             , oneLayer (Just addr) map.centre map.tileSource map.windowSize 0 x2]
                    
oneLayer : Maybe (S.Address (Progress)) -> GeoPoint -> TileSource -> (Int, Int) -> Int -> Int -> Html 
oneLayer maybAddr centre tileSource window dz zoom =
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
        attrs = loadingAttrs maybAddr tileCounts zoom
        displ = M.withDefault "inline-block" <| M.map (\_ -> "none") maybAddr
        mapEl = flowTable zoomTileSize (renderOneTile attrs displ zoom zoomTileSize tileSource.tileUrl) tileRows
    in applyPosition mapEl offset

loadingAttrs : Maybe (S.Address (Progress)) -> (Int, Int) -> Int -> List Attribute
loadingAttrs maybAddr tileCounts zoom = M.withDefault [] <| 
                                        M.map (\addr -> 
                                               [ onLoad addr zoom (1.0 / (toFloat (T.combine (*) tileCounts))) ]
                                              ) maybAddr

onLoad : S.Address (Progress) -> Int -> Float -> Attribute
onLoad addr zoom prog = on "load" (J.succeed (zoom, prog)) (S.message addr)

scale : Int -> Int -> Int
scale dz a = if dz > 0 then a // (2 ^ dz) else a * (2 ^ (-1 * dz))

applyPosition : Html -> (Int, Int) -> Html
applyPosition el pixels =
    let attr = style ((position pixels) ++ absolute) 
    in div [attr] [el]

-- use the model to render a single tile
renderOneTile : List Attribute -> String -> Int -> Int -> TileUrl -> Tile -> Html
renderOneTile attrs display zoom tileSize url tile =
    Html.img (attrs ++ [Attr.src (url zoom tile), Attr.style (("display", display) :: (dimensions (tileSize, tileSize)))]) []
    
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

flowTable : Int -> (a -> Html) -> List (List a) -> Html
flowTable tileSize renderer arr = 
    let row els = Html.div [style (zeroMargin ++ [("white-space", "nowrap"), ("height", px tileSize)])] (map renderer els)
    in div [] (map row arr)

-- supporting functions
vid = flip (//)
mer = flip (%)

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

rows : (a -> b -> c) -> (List a, List b) -> List (List c)
rows f (xs, ys) = map (\y -> map (\x -> (f x y)) xs) ys