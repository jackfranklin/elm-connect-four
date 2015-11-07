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
    board: Board,
    currentPlayer: Colour,
    currentMoveCol: Int
  }

initialModel : Model
initialModel =
  {
    board = createBoard,
    currentPlayer = Red,
    currentMoveCol = 0
  }

-- UPDATE

type Action = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

-- VIEW

drawCell : Cell -> Form
drawCell cell =
  circle 30
    |> filled red
    -- the -3 means the middle column ends up centred on the board
    |> moveX (toFloat (cell.x - 3) * 80)
    |> moveY (toFloat (cell.y - 3) * 80)

drawBoard : Board -> Form
drawBoard board =
  List.map drawCell board |> group

drawBackground : (Float, Float) -> Form
drawBackground (w, h) =
  rect w h |> filled gray

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    (w', h') = (toFloat w, toFloat h)
  in
     collage w h [
       drawBackground (w', h'),
       drawBoard model.board
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
