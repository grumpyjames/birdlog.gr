import Graphics.Element (Element)
import Mouse
import Signal as S
import Text (plainText)

main = S.map toEl movement

toEl : a -> Element
toEl c = plainText <| toString c

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