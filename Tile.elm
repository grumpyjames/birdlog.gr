import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, image, middle, layers, spacer)
import Keyboard (arrows)
import List as L
import Mouse
import Signal as S
import Text (plainText)
import Window

main : Signal Element
main = S.map2 render Window.dimensions movement

type alias Tile = { point: (Int, Int), position: (Float, Float) }

render : (Int, Int) -> (Int, Int) -> Element
render (winX, winY) moves = layers <| [ clg ttf (winX, winY) moves, spacer winX winY ] 

clg : (Int -> Tile -> Form) -> (Int, Int) -> (Int, Int) -> Element
clg renderer (winx, winy) shift = 
    let size = 256
        grid = coords 9 7
        tiles = L.map (step size (offset size shift)) grid
    in collage winx winy <| L.map (renderer size) <| tiles

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

ttf : Int -> Tile -> Form
ttf size t = 
    let content = toString t.point ++ "\n" ++ toString t.position
    in move t.position <| toForm <| tileEl size t content

tileEl : Int -> Tile -> String -> Element
tileEl sz tile _ = osmTile sz tile.point

mapT : (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

mergeT : (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
mergeT op (x1, y1) (x2, y2) = (op x1 x2, op y1 y2) 

addT = mergeT (+)

step : Int -> Tile -> (Int, Int) -> Tile
step size offsetTile baseCoordinate = 
    let basePosition = mapT (toFloat << ((*) size)) baseCoordinate
    in Tile (addT offsetTile.point baseCoordinate) (addT offsetTile.position basePosition) 

osmUrl : Int -> (Int, Int) -> String
osmUrl zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString (x+10)) ++ "/" ++ (toString ((1-y)+10)) ++ ".png"

osmTile : Int -> (Int, Int) -> Element
osmTile size point = image size size <| osmUrl 5 point  

offset : Int -> (Int, Int) -> Tile
offset tileSize (x, y)  = 
    let wraps t = (0 - t) // tileSize
        pos t = toFloat <| t % tileSize
    in Tile (wraps x, wraps y) (pos x, pos y)

coords : Int -> Int -> List (Int, Int)
coords xc yc = 
    let coordRange t = [(( 1 - t ) // 2)..((t - 1) // 2)]
    in cartesianProduct (coordRange xc) (coordRange yc)

cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ L.map ((,) z) ys
      [] -> []

-- simplified drags
movement = S.map2 (addT) keyMovement dragMovement

zeroT = (0, 0)

keyMovement : Signal (Int, Int)
keyMovement = let toTuple a = (a.x, a.y)
              in S.map (mapT ((*) (-256))) <| S.foldp (addT) zeroT <| S.map toTuple arrows

step2 : (Bool, (Int, Int)) -> (Bool, (Int, Int)) -> (Int, Int)
step2 (_, (x2, y2)) (wasDown, (lastx, lasty)) =
    if wasDown then (x2 - lastx, lasty - y2) else zeroT

dragMovement : Signal (Int, Int)
dragMovement = S.foldp addT zeroT <| foldpT step2 ((False, zeroT), zeroT) pnWhenDown

foldpT : (a -> a -> b) -> (a, b) -> Signal a -> Signal b
foldpT fn initState sgnl =
    let glue f newB (oldB, _) = (newB, f newB oldB)
    in S.map snd <| S.foldp (glue fn) initState sgnl 

pnWhenDown : Signal (Bool, (Int, Int))
pnWhenDown = S.map2 (,) Mouse.isDown Mouse.position