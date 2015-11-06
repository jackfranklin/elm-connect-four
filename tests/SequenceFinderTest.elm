module SequenceFinderTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (
  fillCellOnBoard, createBoard, Colour(..),
  sequenceInRow, sequenceInColumn, longestSequenceOnBoard)

sequenceInRowTestOne =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 0 Red
    finalBoard = fillCellOnBoard secondBoard 3 0 Yellow
    expected = { sequence = 2, colour = Red }
  in
    test "sequence in row test one"
    (assertEqual expected (sequenceInRow finalBoard 0))

sequenceInRowTestTwo =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 0 Red
    thirdBoard = fillCellOnBoard secondBoard 2 0 Yellow
    fourthBoard = fillCellOnBoard thirdBoard 3 0 Yellow
    finalBoard = fillCellOnBoard fourthBoard 4 0 Yellow
    expected = { sequence = 3, colour = Yellow }
  in
    test "sequence in row test two"
    (assertEqual expected (sequenceInRow finalBoard 0))

sequenceInRowTestThree =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 0 Red
    thirdBoard = fillCellOnBoard secondBoard 2 0 Yellow
    fourthBoard = fillCellOnBoard thirdBoard 3 0 Red
    fifthBoard = fillCellOnBoard fourthBoard 4 0 Red
    finalBoard = fillCellOnBoard fifthBoard 5 0 Red
    expected = { sequence = 3, colour = Red }
  in
    test "sequence in row test three"
    (assertEqual expected (sequenceInRow finalBoard 0))

sequenceInColumTestOne =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 0 1 Red
    finalBoard = fillCellOnBoard secondBoard 0 3 Yellow
    expected = { sequence = 2, colour = Red }
  in
    test "sequence in column test one"
    (assertEqual expected (sequenceInColumn finalBoard 0))

sequenceInColumnTestTwo =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 0 1 Red
    thirdBoard = fillCellOnBoard secondBoard 0 2 Yellow
    fourthBoard = fillCellOnBoard thirdBoard 0 3 Yellow
    finalBoard = fillCellOnBoard fourthBoard 0 4 Yellow
    expected = { sequence = 3, colour = Yellow }
  in
    test "sequence in column test two"
    (assertEqual expected (sequenceInColumn finalBoard 0))

sequenceInColumnTestThree =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 0 1 Red
    thirdBoard = fillCellOnBoard secondBoard 0 2 Yellow
    fourthBoard = fillCellOnBoard thirdBoard 0 3 Red
    fifthBoard = fillCellOnBoard fourthBoard 0 4 Red
    finalBoard = fillCellOnBoard fifthBoard  0 5 Red
    expected = { sequence = 3, colour = Red }
  in
    test "sequence in column test three"
    (assertEqual expected (sequenceInColumn finalBoard 0))


reverseFillCellOnBoard x y colour board =
  fillCellOnBoard board x y colour

longestSequenceOnBoardTestOne =
  let
    board =
      createBoard
       |> reverseFillCellOnBoard 0 0 Red
       |> reverseFillCellOnBoard 1 0 Red
       |> reverseFillCellOnBoard 2 0 Red
       |> reverseFillCellOnBoard 2 2 Yellow
       |> reverseFillCellOnBoard 3 2 Yellow
       |> reverseFillCellOnBoard 4 2 Yellow
       |> reverseFillCellOnBoard 5 2 Yellow
    expected = { sequence = 4, colour = Yellow }
  in
    test "it finds a long row sequence"
    (assertEqual expected (longestSequenceOnBoard board))

longestSequenceOnBoardTestTwo =
  let
    board =
      createBoard
       |> reverseFillCellOnBoard 0 0 Red
       |> reverseFillCellOnBoard 0 1 Red
       |> reverseFillCellOnBoard 0 2 Red
       |> reverseFillCellOnBoard 2 2 Yellow
       |> reverseFillCellOnBoard 2 3 Yellow
       |> reverseFillCellOnBoard 2 4 Yellow
       |> reverseFillCellOnBoard 2 5 Yellow
    expected = { sequence = 4, colour = Yellow }
  in
    test "it finds a long column sequence on the board"
    (assertEqual expected (longestSequenceOnBoard board))

longestSequenceOnBoardTestThree =
  let
    board =
      createBoard
       |> reverseFillCellOnBoard 0 0 Red
       |> reverseFillCellOnBoard 0 1 Red
       |> reverseFillCellOnBoard 0 2 Red
       |> reverseFillCellOnBoard 1 1 Yellow
       |> reverseFillCellOnBoard 2 2 Yellow
       |> reverseFillCellOnBoard 3 3 Yellow
       |> reverseFillCellOnBoard 4 4 Yellow
    expected = { sequence = 4, colour = Yellow }
  in
    test "it finds a long diagonal sequence on the board"
    (assertEqual expected (longestSequenceOnBoard board))


tests : Test
tests =
    suite "SequenceFinderTest"
      [
        suite "sequenceInRow"
        [
          sequenceInRowTestOne, sequenceInRowTestTwo, sequenceInRowTestThree
        ],
        suite "sequenceInColumn"
        [
          sequenceInColumTestOne, sequenceInColumnTestTwo, sequenceInColumnTestThree
        ],
        suite "longestSequenceOnBoard"
        [
          longestSequenceOnBoardTestOne, longestSequenceOnBoardTestTwo, longestSequenceOnBoardTestThree
        ]
      ]
