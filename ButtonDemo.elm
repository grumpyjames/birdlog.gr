module ButtonDemo (main) where

import Graphics.Element (Element, flow, right)
import Graphics.Input (button)
import Signal (Channel, Signal, channel, foldp, map, send, subscribe)
import Text (plainText)

type ZoomChange = In | Out | None

zoomChange = channel None

zoomIn = button (send zoomChange In) "+"
zoomOut = button (send zoomChange Out) "-"

main : Signal Element
main = 
    let zs = subscribe zoomChange
        step zc z = 
            case zc of
              In -> z + 1
              Out -> z - 1
              None -> z
        currentZoom = foldp step 0 zs
    in map (\a -> flow right [plainText <| toString a, zoomIn, zoomOut]) currentZoom