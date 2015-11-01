module SequenceFinder where

import ConnectFour exposing(Board, Colour(..), findCellOnBoard, maxXValue, maxYValue)
import List.Extra exposing(group, maximumBy)

type alias SequenceMemo =
  {
    last: Colour,
    current: Int,
    longest: Int,
    sequenceColour: Colour
  }

type alias SequenceResult =
  { colour : Colour, sequence : Int }


initialMemo : SequenceMemo
initialMemo =
  { sequenceColour = NoColour, last = NoColour, current = 0, longest = 0 }


getNewCoordsForDirection: String -> (Int, Int) -> (Int, Int)
getNewCoordsForDirection direction (x, y) =
  case direction of
    "up-right" -> (x + 1, y + 1)
    "down-right" -> (x + 1, y - 1)
    "down-left" -> (x - 1, y - 1)
    "up-left" -> (x - 1, y + 1)

getBoundsForDirection: String -> (Int, Int)
getBoundsForDirection direction =
  case direction of
    "up-right" -> (maxXValue, maxYValue)
    "down-right" -> (maxXValue, 0)
    "down-left" -> (0, 0)
    "up-left" -> (0, maxYValue)


xOrYAtBounds: String -> (Int, Int) -> Bool
xOrYAtBounds direction (x, y) =
  x == (fst (getBoundsForDirection direction)) || y == (snd (getBoundsForDirection direction))

getNextDiagonalCoords cellList direction startX startY =
  case List.head (List.reverse cellList) of
    Nothing -> getNextDiagonalCoords [(startX, startY)] direction startX startY
    Just (lastX, lastY) ->
      if | xOrYAtBounds direction (lastX, lastY) -> cellList
         | otherwise ->
             let
               newList = List.append cellList [getNewCoordsForDirection direction (lastX, lastY)]
             in
               getNextDiagonalCoords newList direction startX startY

buildDiagonalFrom : Int -> Int -> String -> List (Int, Int)
buildDiagonalFrom x y direction =
  getNextDiagonalCoords [] direction x y

colourFromCoords : Board -> Int -> Int -> Colour
colourFromCoords board x y =
  .colour (findCellOnBoard board x y)

getDiagonalsFromCell : Board -> Int -> Int -> List (List Colour)
getDiagonalsFromCell board x y =
  let
    coordsToColour (cX, cY) =
      colourFromCoords board cX cY
  in
    [
      (List.map coordsToColour (buildDiagonalFrom x y "up-right")),
      (List.map coordsToColour (buildDiagonalFrom x y "down-right")),
      (List.map coordsToColour (buildDiagonalFrom x y "up-left")),
      (List.map coordsToColour (buildDiagonalFrom x y "down-left"))
    ]


getRowOfColours : Board -> Int -> List Colour
getRowOfColours board yIndex =
  board
    |> List.filter (\cell -> cell.y == yIndex)
    |> List.map .colour

getColumnOfColours : Board -> Int -> List Colour
getColumnOfColours board xIndex =
  board
    |> List.filter (\cell -> cell.x == xIndex)
    |> List.map .colour

processListOfColours : List Colour -> SequenceResult
processListOfColours colours =
  let longestSeq = group colours
      |> List.filter (\(colour::_) -> colour /= NoColour)
      |> maximumBy List.length
      |> Maybe.withDefault []
  in
     case List.head longestSeq of
       Nothing -> { sequence = 0, colour = NoColour }
       Just colour -> { sequence = List.length longestSeq, colour = colour }

sequenceInColumn: Board -> Int -> SequenceResult
sequenceInColumn board xIndex =
  processListOfColours (getColumnOfColours board xIndex)

sequenceInRow : Board -> Int -> SequenceResult
sequenceInRow board yIndex =
  processListOfColours (getRowOfColours board yIndex)

sequenceInDiagonal: Board -> Int -> Int -> SequenceResult
sequenceInDiagonal board x y =
  -- there are 4 diagonals for a given cell that might have a sequence
  -- so we get them all and sort to find the one with the longest
  -- which is all we really care about
  List.map processListOfColours (getDiagonalsFromCell board x y)
    |> List.sortBy .sequence
    |> List.reverse
    |> List.head
    |> Maybe.withDefault { sequence = 0, colour = NoColour }

