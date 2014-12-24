import Keyboard
import Mouse
import Window

main : Signal Element
main = lift3 clg Window.dimensions movement (constant hubris)

clg : (Int, Int) -> (Int, Int) -> [Element] -> Element
clg (winx, winy) (xshift, yshift) es = collage winx winy (foldx snd (step xshift yshift) ((-1,-1), []) es)

foldx : (b -> c) -> (a -> b -> b) -> b -> [a] -> c
foldx f step init xs = f <| foldl step init xs

step : Int -> Int -> Element -> ((Int, Int), [Form]) -> ((Int, Int), [Form])
step xoff yoff e ((x, y), fs) = 
    let xpos = (toFloat xoff) + (toFloat (x * 200))
        ypos = (toFloat yoff) + (toFloat (y * 200))
    in ((x+1, y+1), fs ++ [move (xpos, ypos) <| toForm e])

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