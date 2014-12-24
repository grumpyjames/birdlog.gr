import Maybe
import Mouse
import Window

main : Signal Element
--main = lift asText <| lift (uncurry paths) <| (pair Window.dimensions dragList)
main = lift (uncurry draw) <| (pair Window.dimensions dragList)

type Drag = {
  dt: Time,
  dx_by_dt: Float,
  dy_by_dt: Float
}

paths : [Drag] -> [(Float, Float)]
paths ds = 
    let nextPoint drag xs =
        let unit = drag.dt * (1/m)
            x_dist drag = unit * drag.dx_by_dt 
            y_dist drag = unit * drag.dy_by_dt
        in xs ++ [(x_dist drag, y_dist drag)]
    in foldl nextPoint [(0, 0)] ds

thickLine : LineStyle
thickLine =
    { defaultLine |
        color <- rgba 123 123 123 0.3,
        width <- 3
    }

draw : (Int, Int) -> [Drag] -> Element
draw (cx, cy) ds = collage cx cy <| [traced thickLine (path (reverse (paths ds)))]

pair : Signal a -> Signal b -> Signal (a,b)
pair = lift2 (,)

dragList : Signal [Drag]
dragList = foldp (f) [] (lift right accs)

f : Maybe a -> [a] -> [a]
f myb xs = 
    case myb of
      Just x -> xs ++ [x]
      Nothing -> xs

c (x, y) = (x // 2, y // 2)

data Acc = Acc Time (Maybe (Int, Int)) (Maybe Drag)

accs : Signal Acc
accs = foldp step' (Acc 0 Nothing Nothing) (timestamp pnWhenDown)

step' : (Time, (Int, Int)) -> Acc -> Acc
step' (t2, (x2, y2)) acc =
    case acc of
      Acc 0 Nothing Nothing -> Acc t2 (Just (x2, y2)) Nothing
      Acc t1 (Just (x1, y1)) _ -> Acc t2 (Just (x2, y2)) (parseDrag (t2 - t1) (x2 - x1) (y1 - y2))                

pnWhenDown = keepWhen Mouse.isDown (0,0) Mouse.position

right : Acc -> Maybe Drag
right acc =
    case acc of Acc t a b -> b

parseDrag : Time -> Int -> Int -> Maybe Drag
parseDrag dt dx dy = if dt == 0 then Nothing else Just (Drag dt (m * (toFloat dx)/dt) (m * (toFloat dy)/dt))

m = toFloat 1000