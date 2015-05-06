import Color exposing (red)
import Graphics.Element exposing (Element, color, container, empty, layers, opacity, show, topLeft)
import Native.Location exposing (location)
import Osm exposing (openStreetMap, simpleOsm)
import Signal exposing (map)
import Text exposing (fromString)
import Types exposing (GeoPoint, Zoom(..))

location = Native.Location.location

main = map shw location

shw : LocationResponse -> Element
shw lr =
    case lr of
      NoneYet -> show "No location yet"
      Error code -> show <| "An error of code: " ++ (toString code)
      LatLn lat lon -> showLocation (Zoom 16) {lat = lat, lon = lon}

showLocation : Zoom -> GeoPoint -> Element
showLocation zoom geopt =
    let offsets = openStreetMap.locate zoom geopt
        tileImg = simpleOsm zoom <| offsets.tile
        pixelOffsets = offsets.position.pixels
        indicator = container (fst pixelOffsets) (snd pixelOffsets) topLeft empty
    in layers <| [ tileImg, opacity 0.3 <| color red <| indicator ] 

type LocationResponse = NoneYet | LatLn Float Float | Error Int