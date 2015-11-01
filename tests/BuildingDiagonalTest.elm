module BuildingDiagonalTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import SequenceFinder exposing (buildDiagonalFrom)

import ConnectFour exposing (createBoard)

diagonalUpRightTest =
  let
    board = createBoard
    expected = [(2, 2), (3, 3), (4, 4), (5, 5)]
  in
    test "up-right finds the expected list"
    (assertEqual expected (buildDiagonalFrom 2 2 "up-right"))

diagonalDownRightTest =
  let
    board = createBoard
    expected = [(3, 3), (4, 2), (5, 1), (6, 0)]
  in
    test "down-right finds the expected list"
    (assertEqual expected (buildDiagonalFrom 3 3 "down-right"))


tests : Test
tests =
    suite "BuildingDiagonalTest"
      [
        diagonalUpRightTest, diagonalDownRightTest
      ]
