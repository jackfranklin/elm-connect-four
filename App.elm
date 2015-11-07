import Keyboard
import Mouse
import Window

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import ConsoleLog exposing (log)

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

type Action =
  NoOp
  | MovePickerLeft
  | MovePickerRight

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    MovePickerLeft ->
      if model.currentMoveCol == 0 then
         model
       else
        { model | currentMoveCol <- (model.currentMoveCol - 1) }
    MovePickerRight ->
      if model.currentMoveCol == maxXValue then
         model
       else
          { model | currentMoveCol <- (model.currentMoveCol + 1) }

-- VIEW

cellColourToGraphic : Colour -> Color
cellColourToGraphic colour =
  case colour of
    Red -> red
    Yellow -> yellow
    _ -> white

drawCell : Cell -> Form
drawCell cell =
  circle 30
    |> filled (cellColourToGraphic cell.colour)
    -- the -3 means the middle column ends up centred on the board
    |> moveX (toFloat (cell.x - 3) * 80)
    |> moveY (toFloat (cell.y - 3) * 80)

drawCounter: Model -> Form
drawCounter model =
  circle 30
    |> filled red
    |> moveX (toFloat (model.currentMoveCol - 3) * 80)
    |> moveY 240.00

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
       drawCounter model,
       drawBoard model.board
     ]


-- SIGNALS

columnPicker : Signal Action
columnPicker =
  let
    x = Signal.map .x Keyboard.arrows
    toAction n =
      case n of
        -1 -> MovePickerLeft
        0  -> NoOp
        1  -> MovePickerRight
  in
    Signal.map toAction x

input : Signal Action
input =
  Signal.mergeMany [columnPicker]


model : Signal Model
model =
  Signal.foldp update initialModel input

main : Signal Element
main =
  Signal.map2 view Window.dimensions model
