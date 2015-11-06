module BuildingDiagonalTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (createBoard, buildDiagonalFrom, DiagonalDirection(..))

diagonalUpRightTest =
  let
    board = createBoard
    expected = [(2, 2), (3, 3), (4, 4), (5, 5)]
  in
    test "up-right finds the expected list"
    (assertEqual expected (buildDiagonalFrom (2, 2) UpRight))

diagonalDownRightTest =
  let
    board = createBoard
    expected = [(3, 3), (4, 2), (5, 1), (6, 0)]
  in
    test "down-right finds the expected list"
    (assertEqual expected (buildDiagonalFrom (3, 3) DownRight))

diagonalDownLeftTest =
  let
    board = createBoard
    expected = [(3, 3), (2, 2), (1, 1), (0, 0)]
  in
    test "down-left finds the expected list"
    (assertEqual expected (buildDiagonalFrom (3, 3) DownLeft))

diagonalUpLeftTest =
  let
    board = createBoard
    expected = [(3, 3), (2, 4), (1, 5)]
  in
    test "up-left finds the expected list"
    (assertEqual expected (buildDiagonalFrom (3, 3) UpLeft))


tests : Test
tests =
    suite "BuildingDiagonalTest"
      [
        diagonalUpRightTest, diagonalDownRightTest,
        diagonalDownLeftTest, diagonalUpLeftTest
      ]
