module Movement (main, deltas, movement) where 

import Graphics.Element (Element)
import Keyboard (arrows)
import Mouse
import Signal as S
import Tuple (..)
import Text (plainText)

main = S.map (\c -> plainText <| toString c) movement

movement = S.map2 (addT) keyMovement dragMovement

keyMovement : Signal (Int, Int)
keyMovement = S.foldp (addT) zeroT (keyState 256)

step2 : (Bool, (Int, Int)) -> (Bool, (Int, Int)) -> (Int, Int)
step2 (_, (x2, y2)) (wasDown, (lastx, lasty)) =
    if wasDown then (x2 - lastx, lasty - y2) else zeroT

dragMovement : Signal (Int, Int)
dragMovement = S.foldp addT zeroT deltas

deltas : Signal (Int, Int)
deltas = foldpT step2 ((False, zeroT), zeroT) mouseState

foldpT : (a -> a -> b) -> (a, b) -> Signal a -> Signal b
foldpT fn initState sgnl =
    let glue f newB (oldB, _) = (newB, f newB oldB)
    in S.map snd <| S.foldp (glue fn) initState sgnl 

keyState : Int -> Signal (Int, Int)
keyState tileSize =
    let toTuple a = (a.x, a.y)
    in S.map (mapT ((*) (-1 * tileSize))) <| S.map toTuple arrows

mouseState : Signal (Bool, (Int, Int))
mouseState = S.map2 (,) Mouse.isDown Mouse.position