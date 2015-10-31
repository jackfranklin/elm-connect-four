module ConnectFour where

import Maybe exposing(Maybe(..))
import Debug

type Colour = Red | Yellow | NoColour

type alias Board = List Cell

-- board is 6 down, 7 across

type alias Cell =
  { x: Int, y: Int, colour: Colour }

cellIsFilled : Cell -> Bool
cellIsFilled cell =
  case cell.colour of
    NoColour -> False
    _ -> True

colourCell : Cell -> Colour -> Cell
colourCell cell newColour =
  { cell | colour <- newColour }

cellMatches : Cell -> Int -> Int -> Bool
cellMatches cell x y =
  cell.x == x && cell.y == y

findCellOnBoard : Board -> Int -> Int -> Cell
findCellOnBoard board x y =
  let
    defaultCell = { x = 0, y = 0, colour = NoColour }
    foundCells = List.filter (\c -> cellMatches c x y) board
  in
    case (List.head foundCells) of
      --TODO: this is probably not idiomatic Elm...
      Nothing -> Debug.crash "Cell you searched for doesn't exist"
      x -> Maybe.withDefault defaultCell x

fillCellOnBoard : Board -> Int -> Int -> Colour -> Board
fillCellOnBoard board x y colour =
  let
    foundCell = findCellOnBoard board x y
    fillCellIfMatches cell =
      if cellMatches cell x y then colourCell cell colour else cell
  in
    List.map fillCellIfMatches board

createBlankCell : Int -> Int -> Cell
createBlankCell x y =
  { x = x, y = y, colour = NoColour }

biggestSequenceOnRow : Board -> Int -> Int
biggestSequenceOnRow board yIndex =
  let
    row =
      board
       |> List.filter (\cell -> cell.y == yIndex)
       |> List.map .colour

    newMemo last newCurrent oldMemo =
      let
        newLongest =
          if | oldMemo.current > oldMemo.longest -> oldMemo.current
             | otherwise -> oldMemo.longest
      in
        { last = last, current = newCurrent, longest = newLongest }

    processCell currentColour memo =
      case memo.last of
        currentColour -> newMemo currentColour (memo.current + 1) memo
        _ -> newMemo currentColour 1 memo

    processRow colour memo =
      case colour of
        NoColour -> newMemo colour 0 memo
        colourType -> processCell colourType memo

  in
    .longest (List.foldl processRow { last = NoColour, current = 0, longest = 0 } row)


createBoard : List Cell
createBoard =
  let
    xValues = [0..6]
    yValues = [0..5]
  in
    List.concatMap (\x -> (List.map (\y -> (createBlankCell x y)) yValues)) xValues
