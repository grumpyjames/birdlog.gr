import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, image, middle)
import Keyboard (arrows)
import List as L
import Mouse
import Signal as S
import Text (plainText)
import Window

main : Signal Element
main = S.map2 clg Window.dimensions movement

type alias Tile = { point: (Int, Int), position: (Float, Float) }

clg : (Int, Int) -> (Int, Int) -> Element
clg (winx, winy) shift = 
    let size = 256
        grid = coords 9 7
        tiles = L.map (step size (offset size shift)) grid
    in collage winx winy <| L.map (ttf size) <| tiles

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

ttf : Int -> Tile -> Form
ttf size t = 
    let content = toString t.point ++ "\n" ++ toString t.position
    in move t.position <| toForm <| tileEl size t content

mapT : (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

mergeT : (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
mergeT op (x1, y1) (x2, y2) = (op x1 x2, op y1 y2) 

step : Int -> Tile -> (Int, Int) -> Tile
step size offsetTile baseCoordinate = 
    let basePosition = mapT (toFloat << ((*) size)) baseCoordinate
        addT = mergeT (+)
    in Tile (addT offsetTile.point baseCoordinate) (addT offsetTile.position basePosition) 

osmUrl : Int -> (Int, Int) -> String
osmUrl zoom (x,y) = "http://tile.openstreetmap.org/" ++ (toString zoom) ++ "/" ++ (toString (x+5)) ++ "/" ++ (toString ((1-y)+5)) ++ ".png"

osmTile : Int -> (Int, Int) -> Element
osmTile size point = image size size <| osmUrl 5 point  

tileEl : Int -> Tile -> String -> Element
tileEl sz tile _ = color grey (container sz sz middle (osmTile sz tile.point))

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
movement = S.map2 (mergeT (+)) keyMovement dragMovement

dragMovement : Signal (Int, Int)
dragMovement = let rawSignal = S.foldp step' (MouseUp, (0, 0)) pnWhenDown
               in S.map snd <| rawSignal

keyMovement : Signal (Int, Int)
keyMovement = let toTuple a = (a.x, a.y)
                  step = mergeT (+)
              in S.map (mapT ((*) (-256))) <| S.foldp (mergeT (+)) (0, 0) <| S.map toTuple arrows

type DragState = MouseDown (Int, Int)
               | MouseUp

step' : (Bool, (Int, Int)) -> (DragState, (Int, Int)) -> (DragState, (Int, Int))
step' (down, (x2, y2)) (lastState, (sumx, sumy)) =
    let newState = if down then MouseDown (x2, y2) else MouseUp
        newSum = case lastState of
                   MouseUp -> (sumx, sumy)
                   MouseDown (lastx, lasty) -> (sumx - x2 + lastx, sumy + y2 - lasty)
    in (newState, newSum)

pnWhenDown : Signal (Bool, (Int, Int))
pnWhenDown = S.map2 (,) Mouse.isDown Mouse.position