module SlippyMap (main) where

import Movement (movement)
import Osm (osm, tileSize)
import Signal as S
import Tile (Model, Zoom(..), render)
import Tuple (..)
import Window

-- 'inverted' mouse, but elm's y and osms are opposite. Do the remaining flips in `step`
main = S.map (render osm tileSize) <| S.map3 Model (S.constant (Zoom 5)) Window.dimensions (S.map (multiplyT (-1, 1)) movement)