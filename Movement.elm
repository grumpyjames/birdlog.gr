module Movement (keyState) where 

import Keyboard exposing (arrows)
import Signal as S

keyState : Signal (Int, Int)
keyState =
    let toTuple a = (a.x, a.y)
    in S.map toTuple arrows