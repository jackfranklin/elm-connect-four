module Main exposing (..)

import Keyboard
import Html exposing (Html)
import Html.App as Html
import Text
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import ConnectFour exposing (..)


-- MODEL


type alias Model =
    { board : Board
    , currentPlayer : Colour
    , currentMoveCol : Int
    , winningColour : Colour
    }


initialModel : Model
initialModel =
    { board = createBoard
    , currentPlayer = Red
    , currentMoveCol = 0
    , winningColour = NoColour
    }



-- UPDATE


type Msg
    = NoOp
    | MovePickerLeft
    | MovePickerRight
    | PlaceCounterBelow
    | RestartGame


makeMove : Model -> Model
makeMove model =
    if columnFull model.currentMoveCol model.board then
        model
    else
        let
            newBoard =
                placeCounter model.currentMoveCol model.currentPlayer model.board

            newPlayer =
                if model.currentPlayer == Red then
                    Yellow
                else
                    Red

            hasWon =
                boardHasWinner newBoard
        in
            { model
                | board = newBoard
                , currentPlayer = newPlayer
                , currentMoveCol = 0
                , winningColour =
                    if hasWon then
                        model.currentPlayer
                    else
                        NoColour
            }


wrapWithCmd : Model -> ( Model, Cmd Msg )
wrapWithCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            wrapWithCmd model

        MovePickerLeft ->
            if model.currentMoveCol == 0 then
                wrapWithCmd model
            else
                wrapWithCmd { model | currentMoveCol = (model.currentMoveCol - 1) }

        MovePickerRight ->
            if model.currentMoveCol == maxXValue then
                wrapWithCmd model
            else
                wrapWithCmd { model | currentMoveCol = (model.currentMoveCol + 1) }

        PlaceCounterBelow ->
            wrapWithCmd (makeMove model)

        RestartGame ->
            wrapWithCmd initialModel



-- VIEW


cellColourToGraphic : Colour -> Color
cellColourToGraphic colour =
    case colour of
        Red ->
            red

        Yellow ->
            yellow

        _ ->
            white


drawCell : Cell -> Form
drawCell cell =
    circle 30
        |>
            filled (cellColourToGraphic cell.colour)
        -- the -3 means the middle column ends up centred on the board
        |>
            moveX (toFloat (cell.x - 3) * 80)
        |>
            moveY (toFloat (cell.y - 3) * 80)


drawCounter : Model -> Form
drawCounter model =
    circle 30
        |> filled (cellColourToGraphic model.currentPlayer)
        |> moveX (toFloat (model.currentMoveCol - 3) * 80)
        |> moveY 240.0


drawBoard : Board -> Form
drawBoard board =
    List.map drawCell board |> group


drawBackground : ( Float, Float ) -> Form
drawBackground ( w, h ) =
    rect w h |> filled gray


drawWinner : ( Float, Float ) -> Colour -> Form
drawWinner ( w, h ) colour =
    case colour of
        NoColour ->
            rect 0 0 |> filled grey

        colour ->
            [ rect (w - 100) (h - 100) |> filled blue
            , Text.fromString ("Winner! " ++ (toString colour))
                |> Text.color white
                |> Text.height 30
                |> text
            , Text.fromString "Press spacebar to restart"
                |> Text.color white
                |> Text.height 15
                |> text
                |> moveY -40
            ]
                |> group


drawBoardBackground : Form
drawBoardBackground =
    rect 560 480 |> filled blue |> moveY -38


view : Model -> Html Msg
view model =
    let
        ( w', h' ) =
            ( toFloat 800, toFloat 800 )
    in
        toHtml
            <| collage 800
                800
                [ drawBackground ( w', h' )
                , drawCounter model
                , drawBoardBackground
                , drawBoard model.board
                , drawWinner ( w', h' ) model.winningColour
                ]


keyboardPresses : Sub Msg
keyboardPresses =
    Keyboard.ups
        (\keyCode ->
            case keyCode of
                30 ->
                    RestartGame

                37 ->
                    MovePickerLeft

                39 ->
                    MovePickerRight

                40 ->
                    PlaceCounterBelow

                _ ->
                    NoOp
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    keyboardPresses


init =
    ( initialModel, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
