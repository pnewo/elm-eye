import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

-- MODEL

type alias Model =
  { rectAngle : Float
  , ngonAngel : Float
  }

initialModel : Model
initialModel =
  { rectAngle = 0.0
  , ngonAngel = 0.0
  } 

-- VIEW

view : Model -> Element
view model = 
    collage 300 300
    [ ngon 4 75
        |> filled clearGrey
        |> move (-10,0)
        |> rotate model.rectAngle
    , ngon 5 50
        |> filled clearGrey
        |> move (50,10)
        |> rotate model.ngonAngel
    ] 

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

-- UPDATE

update : Float -> Model -> Model
update time model =
  { model
  | rectAngle = model.rectAngle + 0.01
  , ngonAngel = model.ngonAngel - 0.06
  }

-- SIGNALS

model : Signal Model
model =
    Signal.foldp update initialModel (Time.fps 30)


main : Signal Element
main =
  Signal.map view model