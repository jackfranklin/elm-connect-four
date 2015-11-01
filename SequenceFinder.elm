module SequenceFinder where

import ConnectFour exposing(Board, Colour(..), findCellOnBoard, maxXValue, maxYValue)

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

getBoundsForDirection: String -> (Int, Int)
getBoundsForDirection direction =
  case direction of
    "up-right" -> (maxXValue, maxYValue)
    "down-right" -> (maxXValue, 0)


getNextDiagonalCoords cellList direction startX startY =
  case List.head (List.reverse cellList) of
    Nothing -> getNextDiagonalCoords [(startX, startY)] direction startX startY
    Just (lastX, lastY) ->
      if | lastX == (fst (getBoundsForDirection direction))
           || lastY == (snd (getBoundsForDirection direction)) -> cellList
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
      List.map coordsToColour (buildDiagonalFrom x y "up-right")
      -- buildDiagonalFrom x y "down-right",
      -- buildDiagonalFrom x y "up-left",
      -- buildDiagonalFrom x y "down-left"
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

updateSequenceMemo : Colour -> Int -> SequenceMemo -> SequenceMemo
updateSequenceMemo latestColour newCurrentCount oldMemo =
  let
    haveNewLongest =
      newCurrentCount > oldMemo.current && newCurrentCount > oldMemo.longest

    newLongest =
      if haveNewLongest then newCurrentCount else oldMemo.longest

    sequenceColour =
      if haveNewLongest then latestColour else oldMemo.sequenceColour
  in
    { last = latestColour,
      current = newCurrentCount,
      longest = newLongest,
      sequenceColour = sequenceColour }

processColour : Colour -> SequenceMemo -> SequenceMemo
processColour colour memo =
  case colour of
    -- if this cell is empty, current sequence is now 0 and we start again
    NoColour -> updateSequenceMemo NoColour 0 memo
    Red -> case memo.last of
      Red -> updateSequenceMemo Red (memo.current + 1) memo
      _ -> updateSequenceMemo Red 1 memo
    Yellow -> case memo.last of
      Yellow -> updateSequenceMemo Yellow (memo.current + 1) memo
      _ -> updateSequenceMemo Yellow 1 memo


processListOfColours : List Colour -> SequenceResult
processListOfColours colours =
  let
    result = List.foldl processColour initialMemo colours
  in
    {
      sequence = result.longest,
      colour = result.sequenceColour
    }

sequenceInColumn: Board -> Int -> SequenceResult
sequenceInColumn board xIndex =
  processListOfColours (getColumnOfColours board xIndex)

sequenceInRow : Board -> Int -> SequenceResult
sequenceInRow board yIndex =
  processListOfColours (getRowOfColours board yIndex)

processDiagonalLists : List (List Colour) -> List SequenceResult
processDiagonalLists diagonalLists =
  List.map (\colourList -> processListOfColours colourList) diagonalLists

sequenceInDiagonal: Board -> Int -> Int -> SequenceResult
sequenceInDiagonal board x y =
  List.map processListOfColours (getDiagonalsFromCell board x y)
    |> List.sortBy .sequence
    |> List.reverse
    |> List.head
    |> Maybe.withDefault { sequence = 0, colour = NoColour }

