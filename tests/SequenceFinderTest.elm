module SequenceFinderTest exposing (..)

import String
import ElmTest exposing (..)
import ConnectFour
    exposing
        ( fillCellOnBoard
        , createBoard
        , Colour(..)
        , sequenceInRow
        , sequenceInColumn
        , longestSequenceOnBoard
        )


sequenceInRowTestOne =
    let
        board =
            createBoard
                |> fillCellOnBoard ( 0, 0 ) Red
                |> fillCellOnBoard ( 1, 0 ) Red
                |> fillCellOnBoard ( 3, 0 ) Yellow

        expected =
            { sequence = 2, colour = Red }
    in
        test "sequence in row test one"
            (assertEqual expected (sequenceInRow board 0))


sequenceInColumnTestOne =
    let
        board =
            createBoard
                |> fillCellOnBoard ( 0, 0 ) Red
                |> fillCellOnBoard ( 0, 1 ) Red
                |> fillCellOnBoard ( 0, 3 ) Yellow

        expected =
            { sequence = 2, colour = Red }
    in
        test "sequence in column test one"
            (assertEqual expected (sequenceInColumn board 0))


longestSequenceOnBoardTestOne =
    let
        board =
            createBoard
                |> fillCellOnBoard ( 0, 0 ) Red
                |> fillCellOnBoard ( 1, 0 ) Red
                |> fillCellOnBoard ( 2, 0 ) Red
                |> fillCellOnBoard ( 2, 2 ) Yellow
                |> fillCellOnBoard ( 3, 2 ) Yellow
                |> fillCellOnBoard ( 4, 2 ) Yellow
                |> fillCellOnBoard ( 5, 2 ) Yellow

        expected =
            { sequence = 4, colour = Yellow }
    in
        test "it finds a long row sequence"
            (assertEqual expected (longestSequenceOnBoard board))


longestSequenceOnBoardTestTwo =
    let
        board =
            createBoard
                |> fillCellOnBoard ( 0, 0 ) Red
                |> fillCellOnBoard ( 0, 1 ) Red
                |> fillCellOnBoard ( 0, 2 ) Red
                |> fillCellOnBoard ( 2, 2 ) Yellow
                |> fillCellOnBoard ( 2, 3 ) Yellow
                |> fillCellOnBoard ( 2, 4 ) Yellow
                |> fillCellOnBoard ( 2, 5 ) Yellow

        expected =
            { sequence = 4, colour = Yellow }
    in
        test "it finds a long column sequence on the board"
            (assertEqual expected (longestSequenceOnBoard board))


longestSequenceOnBoardTestThree =
    let
        board =
            createBoard
                |> fillCellOnBoard ( 0, 0 ) Red
                |> fillCellOnBoard ( 0, 1 ) Red
                |> fillCellOnBoard ( 0, 2 ) Red
                |> fillCellOnBoard ( 2, 2 ) Yellow
                |> fillCellOnBoard ( 2, 3 ) Yellow
                |> fillCellOnBoard ( 2, 4 ) Yellow
                |> fillCellOnBoard ( 2, 5 ) Yellow

        expected =
            { sequence = 4, colour = Yellow }
    in
        test "it finds a long diagonal sequence on the board"
            (assertEqual expected (longestSequenceOnBoard board))


tests : Test
tests =
    suite "SequenceFinderTest"
        [ suite "sequenceInRow" [ sequenceInRowTestOne ]
        , suite "sequenceInColumn" [ sequenceInColumnTestOne ]
        , suite "longestSequenceOnBoard"
            [ longestSequenceOnBoardTestOne
            , longestSequenceOnBoardTestTwo
            , longestSequenceOnBoardTestThree
            ]
        ]
