module ConnectFour where

import Maybe exposing(Maybe(..))
import Debug

import List.Extra exposing(group, maximumBy)

type Colour = Red | Yellow | NoColour

type alias Board = List Cell

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


-- board is 6 down, 7 across
maxXValue = 6
maxYValue = 5

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

cellMatches: Coord -> Cell -> Bool
cellMatches (x, y) cell =
  cell.x == x && cell.y == y

findCellOnBoard : Board -> Int -> Int -> Cell
findCellOnBoard board x y =
  case List.head (List.filter (cellMatches (x, y)) board) of
    --TODO: this is probably not idiomatic Elm...
    Nothing -> Debug.crash "Cell you searched for doesn't exist"
    Just x -> x

fillCellOnBoard : Coord -> Colour -> Board -> Board
fillCellOnBoard (x, y) colour board =
  let
    fillCellIfMatches cell =
      if cellMatches (x, y) cell then colourCell cell colour else cell
  in
    List.map fillCellIfMatches board

createBlankCell : Coord -> Cell
createBlankCell (x, y) =
  { x = x, y = y, colour = NoColour }

createBoard : List Cell
createBoard =
  let
    xValues = [0..maxXValue]
    yValues = [0..maxYValue]
  in
    List.concatMap (\x -> (List.map (\y -> (createBlankCell (x, y))) yValues)) xValues

boardHasWinner : Board -> Bool
boardHasWinner board =
  .sequence (longestSequenceOnBoard board) == 4

-- stuff from here down is finding sequences
-- and needs to be split out really
-- but that introduces a circular dependency
-- the sep of concerns isn't right here
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

colourFromCoords : Board -> Coord -> Colour
colourFromCoords board (x, y) =
  .colour (findCellOnBoard board x y)

getDiagonalsFromCell : Board -> Int -> Int -> List (List Colour)
getDiagonalsFromCell board x y =
  List.map (\direction ->
    List.map (colourFromCoords board) (buildDiagonalFrom (x, y) direction)
  ) [UpRight, DownRight, UpLeft, DownLeft]


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
  findLargestSequence <| List.map processListOfColours (getDiagonalsFromCell board x y)

findLargestSequence : List SequenceResult -> SequenceResult
findLargestSequence sequences =
  sequences
    |> List.sortBy .sequence
    |> List.reverse
    |> List.head
    |> Maybe.withDefault { sequence = 0, colour = NoColour }

longestSequenceOnBoard : Board -> SequenceResult
longestSequenceOnBoard board =
  -- TODO: slightly inefficient, if we find a row why bother with columns and diagonals
  findLargestSequence <| List.concat [
    List.map (sequenceInRow board) [0..maxYValue],
    List.map (sequenceInColumn board) [0..maxXValue],
    List.concatMap (\x ->
      List.map (sequenceInDiagonal board x) [0..maxYValue]
    ) [0..maxXValue]
  ]
