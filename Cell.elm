import Keyboard
import Mouse
import Window

main : Signal Element
main = lift2 clg Window.dimensions movement

type Tile = { x: Int, y: Int, xpos: Float, ypos: Float}

clg : (Int, Int) -> (Int, Int) -> Element
clg (winx, winy) (xshift, yshift) = 
    let grid = coords 3 3
        tiles = map (step xshift yshift) grid
    in collage winx winy <| map ttf <| map wrap tiles

wrap : Tile -> Tile
wrap t = wrapY (wrapX t)

wrapX : Tile -> Tile
wrapX t = if t.xpos > 400 
          then Tile (t.x - 4) t.y (t.xpos - 600) t.ypos
          else t

wrapY : Tile -> Tile
wrapY t = if t.ypos > 400
          then Tile t.x (t.y - 4) t.xpos (t.ypos - 600)
          else t

ttf : Tile -> Form
ttf t = 
    let content = show t.x ++ "," ++ show t.y ++ "\n" ++ show t.xpos ++ "," ++ show t.ypos
    in move (t.xpos, t.ypos) <| toForm <| tileEl content

step : Int -> Int -> (Int, Int) -> Tile
step xoff yoff (x, y) = 
    let xpos = (toFloat xoff) + (toFloat (x * 200))
        ypos = (toFloat yoff) + (toFloat (y * 200))
    in Tile x y xpos ypos

tileEl : String -> Element
tileEl s = color grey (container 200 200 middle (plainText s))

coords : Int -> Int -> [(Int, Int)]
coords xc yc = 
    let xcoords = [-1..(xc - 2)]
        ycoords = [-1..(yc - 2)]
    in cartesianProduct xcoords ycoords

cartesianProduct : [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = 
    case xs of
      z :: zs -> (cartesianProduct zs ys) ++ map ((,) z) ys
      [] -> []

data Direction = Up | Down | Left | Right | None

add : {x: Int, y: Int} -> (Int, Int) -> (Int, Int)
add w (c, d) = (w.x + c, w.y + d)

displacement : Signal {x: Int, y: Int} -> Signal (Int, Int)
displacement = foldp add (-1, -1)

hubris : [Element]
hubris = repeat 3 <| color grey (container 200 200 middle (plainText "Try this with html!"))

-- simplified drags

movement : Signal (Int, Int)
movement = let rawSignal = foldp step' (MouseUp, (0, 0)) pnWhenDown
           in lift snd <| rawSignal

data AccSt = MouseDown (Int, Int)
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
pnWhenDown = lift2 (,) Mouse.isDown Mouse.position