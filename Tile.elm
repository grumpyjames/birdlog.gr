module Tile (render) where

import Types exposing (Model, Position, Tile, TileUrl, Zoom(..))
import Functions exposing (andThen)
import Graphics.Collage exposing (Form, collage, move, toForm)
import Graphics.Element exposing (Element, image, layers, spacer)
import List exposing (concatMap, map)
import Tuple as T

render : (Int, Int) -> Model -> Element
render window m =
    let requiredTiles dim = (3 * m.tileSource.tileSize + dim) // m.tileSource.tileSize
        tileCounts = T.map requiredTiles window
        mapCentre = m.tileSource.locate m.zoom m.centre
        originTileOffset = originOffset m.tileSource.tileSize tileCounts mapCentre.position
        originTile = origin mapCentre.tile tileCounts               
        tiles = cartesianProduct Tile <| T.merge range originTile.coordinate tileCounts
        tileToElement = renderOneTile m
        placeTile = placeOneTile m.tileSource.tileSize originTile originTileOffset
        drawOneTile = tileToElement `andThen` placeTile
     in layers [ 
                  (uncurry collage) window <| map drawOneTile tiles,
                  (uncurry spacer) window
               ]

-- use the model to render a single tile
renderOneTile : Model -> Tile -> Element
renderOneTile model tile = 
    let tileSize = model.tileSource.tileSize
        url = model.tileSource.tileUrl
    in image tileSize tileSize <| url model.zoom tile

-- which tile should go in the top left hand corner?
origin : Tile -> (Int, Int) -> Tile
origin centreTile tileCounts = 
    Tile <| centreTile.coordinate `T.subtract` (T.map (vid 2) tileCounts)

-- How far should the origin tile's top left corner be from the viewport's?
originOffset : Int -> (Int, Int) -> Position -> Position
originOffset tileSize tileCounts centrePixel =
    let pixelOffsets = (128, 128) `T.subtract` centrePixel.pixels
        originPixelOffsets = T.map (\a -> tileSize * (a // -2)) tileCounts
    in Position <| flipY <| originPixelOffsets `T.add` pixelOffsets 

-- appropriately place this tile's element representation 
placeOneTile : Int -> Tile -> Position -> Tile -> Element -> Form
placeOneTile tileSize originTile globalOffset tile element =
    let tileSpecificOffset = relativeTilePosition tileSize originTile tile
        distance = addP globalOffset tileSpecificOffset
    in move (T.map toFloat distance.pixels) <| toForm element

relativeTilePosition : Int -> Tile -> Tile -> Position
relativeTilePosition tileSize originTile tile = 
    let relativeTile = tile.coordinate `T.subtract` originTile.coordinate
        position = flipY <| T.map ((*) tileSize) <| relativeTile
    in Position position

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

cartesianProduct : ((a,b) -> c) -> (List a, List b) -> List c
cartesianProduct f (xs,ys) =
    concatMap (\y -> map (\x -> f (x, y)) xs) ys