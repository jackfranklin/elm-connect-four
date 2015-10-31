module ConnectFour where

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

createBlankCell : Int -> Int -> Cell
createBlankCell x y =
  { x = x, y = y, colour = NoColour }

createBoard : List Cell
createBoard =
  let
    xValues = [0..6]
    yValues = [0..5]
  in
    List.concatMap (\x -> (List.map (\y -> (createBlankCell x y)) yValues)) xValues
