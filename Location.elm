import Graphics.Element (Element)
import Native.Location (location)
import Osm (centeredOn)
import Signal (map)
import Text (..)

main = map show Native.Location.location

show : LocationResponse -> Element
show lr =
    case lr of
      NoneYet -> plainText "No location yet"
      Error code -> plainText <| "An error of code: " ++ (toString code)
      LatLn lat lon -> centeredOn 5 lat lon

type LocationResponse = NoneYet | LatLn Float Float | Error Int