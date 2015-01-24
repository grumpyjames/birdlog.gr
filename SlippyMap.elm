module SlippyMap (main) where

import Movement (movement)
import Osm (osm)
import Signal as S
import Tile (render)
import Window

main = S.map2 (render osm) Window.dimensions movement