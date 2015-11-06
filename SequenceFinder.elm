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

type DiagonalDirection =
  UpRight | DownRight | DownLeft | UpLeft

type alias Coord = (Int, Int)

initialMemo : SequenceMemo
initialMemo =
  { sequenceColour = NoColour, last = NoColour, current = 0, longest = 0 }


getNewCoordsForDirection : DiagonalDirection -> Coord -> Coord
getNewCoordsForDirection direction (x, y) =
  case direction of
    UpRight ->
      (x + 1, y + 1)
    DownRight ->
      (x + 1, y - 1)
    DownLeft ->
      (x - 1, y - 1)
    UpLeft ->
      (x - 1, y + 1)


getBoundsForDirection : DiagonalDirection -> Coord
getBoundsForDirection direction =
  case direction of
    UpRight ->
      (maxXValue, maxYValue)
    DownRight ->
      (maxXValue, 0)
    DownLeft ->
      (0, 0)
    UpLeft ->
      (0, maxYValue)


xOrYAtBounds : DiagonalDirection -> Coord -> Bool
xOrYAtBounds direction (x, y) =
  let coords = getBoundsForDirection direction
  in
    x == (fst coords) || y == (snd coords)


getNextDiagonalCoords : List Coord -> DiagonalDirection -> Coord -> List Coord
getNextDiagonalCoords cellList direction (startX, startY) =
  case List.head (List.reverse cellList) of
    Nothing -> getNextDiagonalCoords [(startX, startY)] direction (startX, startY)
    Just (lastX, lastY) ->
      if | xOrYAtBounds direction (lastX, lastY) -> cellList
         | otherwise ->
             let
               newList = List.append cellList [getNewCoordsForDirection direction (lastX, lastY)]
             in
               getNextDiagonalCoords newList direction (startX, startY)

buildDiagonalFrom : Coord -> DiagonalDirection -> List Coord
buildDiagonalFrom (x, y) direction =
  getNextDiagonalCoords [] direction (x, y)

colourFromCoords : Board -> Int -> Int -> Colour
colourFromCoords board x y =
  .colour (findCellOnBoard board x y)

getDiagonalsFromCell : Board -> Int -> Int -> List (List Colour)
getDiagonalsFromCell board x y =
  let coordsToColour (cX, cY) =
      colourFromCoords board cX cY
  in
    [
      (List.map coordsToColour (buildDiagonalFrom (x, y) UpRight)),
      (List.map coordsToColour (buildDiagonalFrom (x, y) DownRight)),
      (List.map coordsToColour (buildDiagonalFrom (x, y) UpLeft)),
      (List.map coordsToColour (buildDiagonalFrom (x, y) DownLeft))
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

