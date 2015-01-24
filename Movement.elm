module Movement (main, movement) where 

import Graphics.Element (Element)
import Keyboard (arrows)
import Mouse
import Signal as S
import Tuple (..)
import Text (plainText)

main = S.map (\c -> plainText <| toString c) movement

movement = S.map2 (addT) keyMovement dragMovement

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