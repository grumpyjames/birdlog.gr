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

type alias Tile = { x: Int, y: Int, xpos: Float, ypos: Float}

clg : (Int, Int) -> (Int, Int) -> Element
clg (winx, winy) (xshift, yshift) = 
    let count = 3
        size = 200
        grid = coords count count
        tiles = L.map (step size xshift yshift) grid
    in collage winx winy <| L.map (ttf size) <| L.map (wrap count size) tiles

wrap : Int -> Int -> Tile -> Tile
wrap count size t = 
    let sgn a = if a > 0 then 1 else -1
        d = toFloat (size * count)
        normalise pos = pos + toFloat (size * (sgn pos))
        wraps pos = truncate <| (normalise pos) / d
        newPos wraps oldPos = oldPos - (d * (toFloat wraps))
        newCoord wraps oldCoord = oldCoord - (count * wraps)
        xWraps = wraps t.xpos
        yWraps = wraps t.ypos
    in Tile (newCoord xWraps t.x) (newCoord yWraps t.y) (newPos xWraps t.xpos) (newPos yWraps t.ypos)

ttf : Int -> Tile -> Form
ttf size t = 
    let content = toString t.x ++ "," ++ toString t.y ++ "\n" ++ toString t.xpos ++ "," ++ toString t.ypos
    in move (t.xpos, t.ypos) <| toForm <| tileEl size content

step : Int -> Int -> Int -> (Int, Int) -> Tile
step size xoff yoff (x, y) = 
    let xpos = (toFloat xoff) + (toFloat (x * size))
        ypos = (toFloat yoff) + (toFloat (y * size))
    in Tile x y xpos ypos

tileEl : Int -> String -> Element
tileEl sz s = color grey (container sz sz middle (plainText s))

coords : Int -> Int -> List (Int, Int)
coords xc yc = 
    let xzeroth = ((xc - 1) // 2)
        minx = 0 - xzeroth
        maxx = xzeroth
        yzeroth = ((yc - 1) // 2)
        miny = 0 - yzeroth
        maxy = yzeroth
        xcoords = [minx..maxx]
        ycoords = [miny..maxy]
    in cartesianProduct xcoords ycoords

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