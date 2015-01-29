module SlippyMap (main) where

import Movement (movement)
import Osm (osm, tileSize)
import Signal as S
import Tile (Zoom(..), render)
import Window

main = S.map2 (render osm tileSize (Zoom 5)) Window.dimensions movement