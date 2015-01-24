import Native.Location (location)
import Signal (map)
import Text (..)

main = map (\l -> centered <| fromString <| toString l) Native.Location.location

type LocationResponse = NoneYet | LatLn Float Float | Error Int