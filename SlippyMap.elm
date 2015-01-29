module SlippyMap (main) where

import Movement (movement)
import Osm (osm, tileSize)
import Signal as S
import Tile (Zoom(..), render)
import Tuple (..)
import Window

-- 'inverted' mouse, but elm's y and osms are opposite. Do the remaining flips in `step`
main = S.map2 (render osm tileSize (Zoom 5)) Window.dimensions (S.map (multiplyT (-1, 1)) movement)