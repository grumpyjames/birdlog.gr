module Tile ( render
            , Progress) where

import Styles exposing (px, absolute, dimensions, noDisplay, position, zeroMargin)
import Model exposing (Model)
import Tuple as T
import Types exposing (GeoPoint, MapState, Position, Tile, TileSource, TileUrl, Zoom(..))

import Array as A exposing (Array)
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
        allLayers = layers addr map
    in wrapper <| List.map oneLayer allLayers

layers : S.Address (Progress) -> (MapState a) -> List ZoomLayer
layers addr m =
    let scale dz a = if dz > 0 then a // (2 ^ dz) else a * (2 ^ (-1 * dz))
    in case m.zoom of
         Constant c -> 
             [ ZoomLayer c (scale 0) Nothing m.tileSource m.windowSize m.centre ]
         Between a b progress -> 
             [ ZoomLayer a (scale (a - b)) Nothing m.tileSource m.windowSize m.centre
             , ZoomLayer b (scale 0) (Just addr) m.tileSource m.windowSize m.centre
             ]
                                                 
type alias ZoomLayer = 
    { zoomToRender : Int
    -- use this to scale the tile size
    , scaler       : Int -> Int
    , loadAddress  : Maybe (S.Address (Progress))
    , tileSource   : TileSource
    , windowSize   : (Int, Int)
    , centre       : GeoPoint
    }

oneLayer : ZoomLayer -> Html 
oneLayer zoomLayer =
    let requiredTiles dim = (3 * zoomLayer.tileSource.tileSize + dim) // zoomLayer.tileSource.tileSize
        tileCounts = T.map requiredTiles zoomLayer.windowSize           
        mapCentre = zoomLayer.tileSource.locate (toFloat zoomLayer.zoomToRender) zoomLayer.centre
        -- scale things based on the differential zoom provided
        zoomWindow = T.map (zoomLayer.scaler) zoomLayer.windowSize
        zoomTileSize = zoomLayer.scaler zoomLayer.tileSource.tileSize
        -- work out how far the desired centre is from the top left of the tiles we will render
        originTile = origin mapCentre.tile tileCounts
        centreTileOffset = mapCentre.tile.coordinate `T.subtract` originTile.coordinate
        relativeCentre = (T.map zoomLayer.scaler mapCentre.position.pixels) `T.add` (T.map ((*) zoomTileSize) centreTileOffset) 
        -- work out how far to shift the map div to make the desired centre the centre
        offset = originOffset zoomLayer.windowSize relativeCentre
        tileRows = rows (curry Tile) <| T.merge range originTile.coordinate tileCounts
        displ = M.withDefault "inline-block" <| M.map (\_ -> "none") zoomLayer.loadAddress
        imageAttrs = Attr.style (("display", displ) :: (dimensions (zoomTileSize, zoomTileSize))) :: loadingAttrs zoomLayer.loadAddress tileCounts zoomLayer.zoomToRender
        rowAttrs = [style (zeroMargin ++ [("white-space", "nowrap"), ("height", px zoomTileSize)])]
        mapEl = flowTable zoomTileSize (renderOneTile imageAttrs zoomLayer.zoomToRender zoomLayer.tileSource.tileUrl) rowAttrs tileRows
    in applyPosition mapEl offset

loadingAttrs : Maybe (S.Address (Progress)) -> (Int, Int) -> Int -> List Attribute
loadingAttrs maybAddr tileCounts zoom = M.withDefault [] <| 
                                        M.map (\addr -> 
                                               [ onLoad addr zoom (1.0 / (toFloat (T.combine (*) tileCounts))) ]
                                              ) maybAddr

onLoad : S.Address (Progress) -> Int -> Float -> Attribute
onLoad addr zoom prog = on "load" (J.succeed (zoom, prog)) (S.message addr)

applyPosition : Html -> (Int, Int) -> Html
applyPosition el pixels =
    let attr = style ((position pixels) ++ absolute) 
    in div [attr] [el]

-- use the model to render a single tile
renderOneTile : List Attribute -> Int -> TileUrl -> Tile -> Html
renderOneTile attrs zoom url tile =
    Html.img (Attr.src (url zoom tile) :: attrs) []
    
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

flowTable : Int -> (a -> Html) -> List Attribute -> Array (Array a) -> Html
flowTable tileSize renderer rowAttrs arr = 
    let row els = Html.div rowAttrs (A.toList <| A.map renderer els)
    in div [] (A.toList <| A.map row arr)

-- supporting functions
vid = flip (//)
mer = flip (%)

range : Int -> Int -> List Int
range origin count = [origin..(origin + count - 1)]

rows : (a -> b -> c) -> (List a, List b) -> Array (Array c)
rows f (xs, ys) =
    let arrX = A.fromList xs
        arrY = A.fromList ys
    in A.map (\y -> A.map (\x -> (f x y)) arrX) arrY