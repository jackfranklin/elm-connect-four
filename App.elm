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
    currentMoveCol: Int,
    winningColour: Colour
  }

initialModel : Model
initialModel =
  {
    board = createBoard,
    currentPlayer = Red,
    currentMoveCol = 0,
    winningColour = NoColour
  }

-- UPDATE

type Action =
  NoOp
  | MovePickerLeft
  | MovePickerRight
  | PlaceCounterBelow

makeMove : Model -> Model
makeMove model =
  if columnFull model.currentMoveCol model.board then
     -- if the column is full NoOp
    model
  else
    -- TODO: check if someone has won on newBoard
    let
      newBoard = placeCounter model.currentMoveCol model.currentPlayer model.board
      newPlayer = if model.currentPlayer == Red then Yellow else Red
    in
      { model | board <- newBoard, currentPlayer <- newPlayer, currentMoveCol <- 0 }


update : Action -> Model -> Model
update action model =
  case (log "action" action) of
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
    PlaceCounterBelow ->
      makeMove model


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
    |> filled (cellColourToGraphic model.currentPlayer)
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

-- TODO: could merge both below signals into one map
columnPicker : Signal Action
columnPicker =
  Signal.filter (\x -> x /= 0) 0 (Signal.map .x Keyboard.arrows)
   |> Signal.map (\n -> if n == -1 then MovePickerLeft else MovePickerRight)


counterPlacer : Signal Action
counterPlacer =
  Signal.filter (\x -> x == -1) 0 (Signal.map .y Keyboard.arrows)
    |> Signal.map (always PlaceCounterBelow)

input : Signal Action
input =
  Signal.mergeMany [columnPicker, counterPlacer]


model : Signal Model
model =
  Signal.foldp update initialModel input

main : Signal Element
main =
  Signal.map2 view Window.dimensions model
