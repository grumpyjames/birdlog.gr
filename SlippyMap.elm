module SlippyMap (main) where

import Movement (movement)
import Signal as S
import Tile (render)
import Window

main = S.map2 render Window.dimensions movement