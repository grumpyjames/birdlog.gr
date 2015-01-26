module SlippyMap (main) where

import Movement (movement)
import Osm (osm, tileSize)
import Signal as S
import Tile (render)
import Window

main = S.map2 (render osm tileSize) Window.dimensions movement