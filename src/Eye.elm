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
view (w, h) (mouseX, mouseY) =
  let
    mouseX' = 2 * toFloat mouseX / toFloat w - 1
    mouseY' = (-2) * toFloat mouseY / toFloat h + 1
    -- -2 * 5 / 5 - 1 = - 1
    -- -2 * 0 / 5 - 1 = 1
    eyeRadius = 50.0
    irisRadius = eyeRadius * 3 / 5
    pupilRadius = irisRadius / 2
    irisX = mouseX' * (eyeRadius - irisRadius)
    irisY = mouseY' * (eyeRadius - irisRadius)
    pupilX = mouseX' * (eyeRadius - pupilRadius)
    pupilY = mouseY' * (eyeRadius - pupilRadius)
  in
    collage w h
      [ circle irisRadius
          |> filled (rgb 107 176 71)
          |> move (irisX, irisY)
      , circle pupilRadius
          |> filled (rgb 50 50 50)
          |> move (pupilX, pupilY)
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
