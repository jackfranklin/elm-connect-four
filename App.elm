module Main exposing (..)

import Keyboard
import Window
import Html exposing (Html)
import Html.App as Html
import Text
import Task
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
    , window : Window.Size
    }


initialModel : Model
initialModel =
    { board = createBoard
    , currentPlayer = Red
    , currentMoveCol = 0
    , winningColour = NoColour
    , window = Window.Size 0 0
    }



-- UPDATE


type Msg
    = NoOp
    | MovePickerLeft
    | MovePickerRight
    | PlaceCounterBelow
    | RestartGame
    | WindowResize Window.Size


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


update : Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        WindowResize size ->
            { model | window = size }

        MovePickerLeft ->
            if model.currentMoveCol == 0 then
                model
            else
                { model | currentMoveCol = (model.currentMoveCol - 1) }

        MovePickerRight ->
            if model.currentMoveCol == maxXValue then
                model
            else
                { model | currentMoveCol = (model.currentMoveCol + 1) }

        PlaceCounterBelow ->
            makeMove model

        RestartGame ->
            initialModel



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
        w =
            model.window.width

        h =
            model.window.height

        ( w', h' ) =
            ( toFloat w, toFloat h )
    in
        toHtml
            <| collage w
                h
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


windowResizes : Sub Msg
windowResizes =
    Window.resizes WindowResize


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keyboardPresses
        , windowResizes
        ]


getWindowSize =
    Task.perform (\_ -> NoOp) WindowResize Window.size


init =
    ( initialModel, getWindowSize )


noCmdUpdateWrap : Msg -> Model -> ( Model, Cmd Msg )
noCmdUpdateWrap msg model =
    ( update msg model, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = noCmdUpdateWrap
        , subscriptions = subscriptions
        }
