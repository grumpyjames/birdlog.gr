module Wheel (Wheel, Component, Unit(..), main) where

import Graphics.Element (Element)
import Native.Wheel
import Signal (map)
import Text (plainText)

main = map (plainText << toString) Native.Wheel.wheel

type Unit = Pixel | Line | Page
type Component = Component Float Unit
type alias Wheel = {
      deltaX : Component,
      deltaY : Component,
      deltaZ : Component
}

