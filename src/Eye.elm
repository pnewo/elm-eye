import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse exposing (position)
import Window exposing (dimensions)

-- VIEW
eyeGlare : Form
eyeGlare =
  circle 7
    |> filled (rgba 255 255 255 0.4)
    |> move (10, 10)

eyeGradient : Float -> Gradient
eyeGradient rad =
  radial (0,0) (rad/3) (0,0) rad
    [ (  0, rgba 255 255 255 0)
    , (  1, rgba 100 100 100 0.2)
    ]

eyeBall : Float -> Form
eyeBall rad =
  circle rad
    |> gradient (eyeGradient rad)


view : (Int, Int) -> (Int, Int) -> Element
view (w, h) (x, y) =
  let
    halfW = toFloat w / 2
    halfH = toFloat h / 2
    dx = toFloat x - toFloat w / 2
    dy = toFloat h / 2 - toFloat y
    eyeRadius = 50.0
    irisDiameter = eyeRadius * 6 / 5
    pupilDiameter = irisDiameter / 2
    eyeAngle = atan2 dy dx
    distance = sqrt (dy ^ 2 + dx ^ 2)
    maxDistance = (min halfH halfW) * 0.8
    distanceCoef = clamp 0 1 (sqrt (distance / maxDistance))
  in
    collage w h
      [ group [
          oval (irisDiameter - 15 * distanceCoef) (irisDiameter)
            |> filled (rgb 107 176 71)
            |> move ((27 * distanceCoef), 0)
        , oval (pupilDiameter - 8 * distanceCoef) (pupilDiameter)
            |> filled (rgb 50 50 50)
            |> move ((35 * distanceCoef), 0)
        ]
        |> rotate eyeAngle
      , eyeGlare
      , eyeBall eyeRadius
      ]

-- SIGNALS
mousePosition : Signal (Int, Int)
mousePosition =
  Mouse.position

windowSize : Signal (Int, Int)
windowSize =
  Window.dimensions

main : Signal Element
main =
  Signal.map2 view windowSize mousePosition
