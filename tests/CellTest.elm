module CellTest exposing (..)

import String
import ElmTest exposing (..)
import ConnectFour exposing (..)


tests : Test
tests =
    suite "Cell Tests"
        [ suite "#cellIsFilled"
            [ test "is true when the cell has a colour"
                (assert (cellIsFilled { x = 1, y = 1, colour = Red }))
            , test "is false when the cell has no colour"
                (assert (not (cellIsFilled { x = 1, y = 1, colour = NoColour })))
            ]
        , suite "#colourCell"
            [ test "it colours the given cell"
                (assertEqual Red (.colour (colourCell { x = 1, y = 1, colour = NoColour } Red)))
            ]
        ]
