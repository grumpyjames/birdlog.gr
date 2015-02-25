module ButtonDemo (main) where

import Color (rgb)
import Graphics.Element (Element, color, container, flow, layers, middle, right, spacer)
import Graphics.Input (button, customButton)
import Signal (Channel, Message, Signal, channel, foldp, map, send, subscribe)
import Text (plainText)

type ZoomChange = In | Out | None

zoomChange = channel None

hoverC = rgb 133 133 133
downC = rgb 99 99 99
upC = rgb 111 111 111

ourButton : Message -> String -> Element
ourButton msg txt = 
    let el = plainText txt
        cn = container 30 30 middle el
        up = color upC cn
        down = color downC cn
        hover = color hoverC cn
    in customButton msg up hover down

zoomIn = ourButton (send zoomChange In) "+"
zoomOut = ourButton (send zoomChange Out) "-"

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