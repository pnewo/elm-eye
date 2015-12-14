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
    mouseX' = toFloat x / halfW - 1
    mouseY' = 1 - toFloat y / halfH
    (dx,dy) =
      (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
    eyeRadius = 50.0
    irisRadius = eyeRadius * 3 / 5
    pupilRadius = irisRadius / 2
    irisX = mouseX' * (eyeRadius - irisRadius)
    irisY = mouseY' * (eyeRadius - irisRadius)
    pupilX = mouseX' * (eyeRadius - pupilRadius)
    pupilY = mouseY' * (eyeRadius - pupilRadius)
    eyeAngle = atan2 dy dx
    eyeAngle2 = atan2 dx dy
    corner = atan2 halfH halfW
    otherCorner = pi - corner
    topBottom = if  (abs (atan2 dy dx) > corner && abs (atan2 dy dx) < otherCorner)
      then True
      else False
    distance = sqrt (dy ^ 2 + dx ^ 2)
    maxDistance = if topBottom then
        abs (halfH / cos ( abs eyeAngle2 ))
      else
        abs (halfW / cos ( abs eyeAngle ))
    distanceCoef = distance / maxDistance
  in
    collage w h
      [ group [
          oval (irisRadius * 2 - 15 * distanceCoef) (irisRadius * 2)
            |> filled (rgb 107 176 71)
            |> move ((27 * distanceCoef), 0)
        , oval (pupilRadius * 2 - 8 * distanceCoef) (pupilRadius * 2)
            |> filled (rgb 50 50 50)
            |> move ((35 * distanceCoef), 0)
        ]
        |> rotate eyeAngle
      , eyeGlare
      , eyeBall eyeRadius
      , toForm (show
          { distance = distance
          , eyeAngle = eyeAngle
          , eyeAngle2 = eyeAngle2
          , maxDistance = maxDistance
          , distanceCoef = distanceCoef
          }
        )
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
