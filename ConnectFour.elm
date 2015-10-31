module ConnectFour where

type Colour = Red | Yellow | NoColour

-- board is 6 down, 7 across

type alias Cell =
  { x: Int, y: Int, colour: Colour }

cellIsFilled : Cell -> Bool
cellIsFilled cell =
  case cell.colour of
    NoColour -> False
    _ -> True

-- createBoard : List Cell
-- create
