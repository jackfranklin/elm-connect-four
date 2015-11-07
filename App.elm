import Keyboard
import Mouse
import Window

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import ConnectFour exposing (..)

-- MODEL

type alias Model =
  {
    cells: Board
  }

initialModel : Model
initialModel =
  { cells = createBoard }

-- UPDATE

type Action = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

-- VIEW
drawBackground : (Float, Float) -> Form
drawBackground (w, h) =
  rect w h |> filled gray

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    (w', h') = (toFloat w, toFloat h)
  in
     collage w h
     [
       drawBackground (w', h')
     ]


-- SIGNALS

input : Signal Action
input =
  Signal.map (always NoOp) Keyboard.arrows

model : Signal Model
model =
  Signal.foldp update initialModel input

main : Signal Element
main =
  Signal.map2 view Window.dimensions model
