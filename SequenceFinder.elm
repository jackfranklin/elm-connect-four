module SequenceFinder where

import ConnectFour exposing(Board, Colour(..))

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

