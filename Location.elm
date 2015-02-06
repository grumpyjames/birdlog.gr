import Color (red)
import Graphics.Element (Element, color, container, empty, layers, opacity, topLeft)
import Native.Location (location)
import Osm (GeoPoint, convert, simpleOsm)
import Signal (map)
import Text (plainText)
import Tile (Zoom(..))
import Tuple (mapT)

location = Native.Location.location

main = map show location

show : LocationResponse -> Element
show lr =
    case lr of
      NoneYet -> plainText "No location yet"
      Error code -> plainText <| "An error of code: " ++ (toString code)
      LatLn lat lon -> showLocation (Zoom 16) {lat = lat, lon = lon}

showLocation : Zoom -> GeoPoint -> Element
showLocation zoom geopt =
    let offsets = convert zoom geopt
        tileImg = simpleOsm zoom <| mapT (\t -> t.index) offsets
        pixelOffsets = mapT (\t -> t.pixel) offsets
        indicator = container (fst pixelOffsets) (snd pixelOffsets) topLeft empty
    in layers <| [ tileImg, opacity 0.3 <| color red <| indicator ] 

type LocationResponse = NoneYet | LatLn Float Float | Error Int