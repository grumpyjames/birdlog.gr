import Color (grey)
import Graphics.Collage (Form, collage, move, toForm)
import Graphics.Element (Element, color, container, middle)
import Keyboard
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
    let size = 200
        grid = coords 9 7
        tiles = L.map (step size (offset size shift)) grid
    in collage winx winy <| L.map (ttf size) <| tiles

sgn a = if a > 0 then 1 else (if a < 0 then -1 else 0) 

ttf : Int -> Tile -> Form
ttf size t = 
    let content = toString t.point ++ "\n" ++ toString t.position
    in move t.position <| toForm <| tileEl size content

mapT : (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

mergeT : (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
mergeT op (x1, y1) (x2, y2) = (op x1 x2, op y1 y2) 

step : Int -> Tile -> (Int, Int) -> Tile
step size offsetTile baseCoordinate = 
    let basePosition = mapT (toFloat << ((*) size)) baseCoordinate
        addT = mergeT (+)
    in Tile (addT offsetTile.point baseCoordinate) (addT offsetTile.position basePosition) 

tileEl : Int -> String -> Element
tileEl sz s = color grey (container sz sz middle (plainText s))

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
movement : Signal (Int, Int)
movement = let rawSignal = S.foldp step' (MouseUp, (0, 0)) pnWhenDown
           in S.map snd <| rawSignal

type AccSt = MouseDown (Int, Int)
           | MouseUp

step' : (Bool, (Int, Int)) -> (AccSt, (Int, Int)) -> (AccSt, (Int, Int))
step' (down, (x2, y2)) (acc, (sumx, sumy)) =
    let oldSum = (sumx, sumy)
    in case (acc, down) of
      (MouseUp, True) -> (MouseDown (x2, y2), oldSum)
      (MouseUp, False) -> (MouseUp, oldSum)
      (MouseDown (lastx, lasty), True) -> (MouseDown (x2, y2), (sumx - x2 + lastx, sumy + y2 - lasty))
      (MouseDown (lastx, lasty), False) -> (MouseUp, oldSum)                                  

pnWhenDown : Signal (Bool, (Int, Int))
pnWhenDown = S.map2 (,) Mouse.isDown Mouse.position