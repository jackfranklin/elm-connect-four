module SequenceFinder where

import ConnectFour exposing(Board, Colour(..))

type alias SequenceMemo  =
  {
    last: Colour,
    current: Int,
    longest: Int,
    sequenceColour: Colour
  }

getRowOfColours : Board -> Int -> List Colour
getRowOfColours board yIndex =
  board
    |> List.filter (\cell -> cell.y == yIndex)
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

sequenceInRow : Board -> Int -> { sequence: Int, colour: Colour }
sequenceInRow board yIndex =
  let
    processRow colour memo =
      case colour of
        -- if this cell is empty, current sequence is now 0 and we start again
        NoColour -> updateSequenceMemo NoColour 0 memo
        Red -> case memo.last of
          Red -> updateSequenceMemo Red (memo.current + 1) memo
          Yellow -> updateSequenceMemo Red 1 memo
          NoColour -> updateSequenceMemo Red 1 memo
        Yellow -> case memo.last of
          Yellow -> updateSequenceMemo Yellow (memo.current + 1) memo
          Red -> updateSequenceMemo Yellow 1 memo
          NoColour -> updateSequenceMemo Yellow 1 memo

    result = List.foldl processRow { sequenceColour = NoColour, last = NoColour, current = 0, longest = 0 } (getRowOfColours board yIndex)
  in
    {
      sequence = result.longest,
      colour = result.sequenceColour
    }

