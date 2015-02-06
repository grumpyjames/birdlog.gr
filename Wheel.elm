module Wheel (Wheel, Unit(..), wheel, main) where

import Graphics.Element (Element)
import Native.Wheel
import Signal (map)
import Text (plainText)

wheel = Native.Wheel.wheel

main = map (plainText << toString) wheel

type Unit = Pixel | Line | Page
type alias Wheel = {
      deltaX : Float,
      deltaY : Float,
      deltaZ : Float,
      unit: Unit
}

