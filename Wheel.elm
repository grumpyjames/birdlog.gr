module Wheel (Wheel, Unit(..), main) where

import Graphics.Element (Element)
import Native.Wheel
import Signal (map)
import Text (plainText)

main = map (plainText << toString) Native.Wheel.wheel

type Unit = Pixel | Line | Page
type alias Wheel = {
      deltaX : Float,
      deltaY : Float,
      deltaZ : Float,
      unit: Unit
}

