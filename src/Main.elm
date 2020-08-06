module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Grid exposing (Grid)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Random
import Sudoku
import Task
import Time
import Util


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- Model


type alias Model =
    { solution : Grid Int
    , puzzle : Grid Int
    , activeCell : Maybe Grid.Cell
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { solution = Grid.init 0
      , puzzle = Grid.init 0
      , activeCell = Nothing
      }
    , Task.perform GotTime Time.now
    )



-- Update


type Msg
    = GotTime Time.Posix
    | CellClicked Grid.Cell
    | NumberSelected Grid.Cell Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime now ->
            let
                seed =
                    Random.initialSeed (Time.posixToMillis now)

                solution =
                    Sudoku.generateSolution seed
            in
            ( { model
                | solution = solution
                , puzzle = Sudoku.createPuzzle solution
              }
            , Cmd.none
            )

        CellClicked cell ->
            let
                activeCell =
                    case model.activeCell of
                        Just oldCell ->
                            if cell == oldCell then
                                Nothing

                            else
                                Just cell

                        Nothing ->
                            Just cell
            in
            ( { model | activeCell = activeCell }
            , Cmd.none
            )

        NumberSelected cell num ->
            ( { model | puzzle = Grid.set cell num model.puzzle, activeCell = Nothing }
            , Cmd.none
            )



-- VIEW


viewPuzzle : Model -> List (Html Msg)
viewPuzzle model =
    List.concatMap (viewRow model) Grid.cellPositions


viewInputNumbers cell =
    div [ class "absolute bottom-0 z-20 flex" ]
        (List.map
            (\num ->
                div
                    [ class "flex items-center justify-center w-20 h-20"
                    , class "bg-white border border-gray-600 cursor-pointer select-none"
                    , class "shadow-md"
                    , Util.onClickStopPropagation (NumberSelected cell num)
                    ]
                    [ text (String.fromInt num) ]
            )
            [ 1, 2, 3, 4 ]
        )


viewRow : Model -> List Grid.Cell -> List (Html Msg)
viewRow { puzzle, activeCell } row =
    List.map
        (\cell ->
            div
                [ class "relative flex items-center justify-center border border-gray-800"
                , class "cursor-pointer select-none"
                , onClick (CellClicked cell)
                ]
                [ text (String.fromInt (Grid.get cell puzzle))
                , case activeCell of
                    Just cell_ ->
                        if cell_ == cell then
                            viewInputNumbers cell

                        else
                            text ""

                    Nothing ->
                        text ""
                ]
        )
        row


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Sudoku-kids"
        [ h1 [ class "py-4 text-4xl text-center" ] [ text "Sudoku-Kids" ]
        , div [ class "w-full" ]
            [ div
                [ class "w-full mx-auto text-lg text-5xl bg-blue-200 grid grid-rows-4 grid-cols-4"
                , style "width" "50vw"
                , style "height" "50vw"
                ]
                (viewPuzzle model)
            ]
        ]
