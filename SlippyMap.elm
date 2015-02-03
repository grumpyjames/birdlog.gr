module SlippyMap (main) where

import Movement (movement)
import Osm (osm, tileSize)
import Signal as S
import Tile (Model, Zoom(..), render)
import Tuple (..)
import Window

-- 'inverted' mouse, but elm's y and osms are opposite. Do any remaining flips below
main = 
    let mapCenter = S.map (multiplyT (-1, 1)) movement
        zoom = S.constant (Zoom 5)
        draw = render osm tileSize
    in S.map draw <| S.map3 Model zoom Window.dimensions mapCenter