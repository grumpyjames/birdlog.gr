module Movement (mouseState, keyState) where 

import Keyboard (arrows)
import Mouse
import Signal as S

keyState : Signal (Int, Int)
keyState =
    let toTuple a = (a.x, a.y)
    in S.map toTuple arrows

mouseState : Signal (Bool, (Int, Int))
mouseState = S.map2 (,) Mouse.isDown Mouse.position