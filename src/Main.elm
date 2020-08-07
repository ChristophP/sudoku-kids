module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Grid exposing (Grid)
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Random
import Sudoku
import Task
import Time
import Util exposing (attrIf, viewIf)


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
    | Restart


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

        Restart ->
            init ()



-- VIEW


viewPuzzle : Model -> List (Html Msg)
viewPuzzle model =
    List.concatMap (viewRow model) Grid.cellPositions


viewInputNumbers : Grid.Cell -> Html Msg
viewInputNumbers cell =
    div
        [ class "flex mx-auto mt-2"
        , style "width" "60vw"
        , style "height" "15vw"
        ]
        (List.map
            (\num ->
                div
                    [ class "flex items-center justify-center flex-1 "
                    , class "bg-white border border-gray-600 cursor-pointer"
                    , class "shadow-md"
                    , Util.onClickStopPropagation (NumberSelected cell num)
                    ]
                    [ text (String.fromInt num) ]
            )
            [ 1, 2, 3, 4 ]
        )


viewRow : Model -> List Grid.Cell -> List (Html Msg)
viewRow { puzzle, solution, activeCell } row =
    List.map
        (\cell ->
            let
                value =
                    Grid.get cell puzzle

                isCorrectValue =
                    value == Grid.get cell solution

                isActiveCell =
                    Just cell == activeCell
            in
            div
                [ class "relative flex items-center justify-center border border-gray-800"
                , attrIf (not isCorrectValue) (class "cursor-pointer")
                , attrIf (not isCorrectValue) (onClick (CellClicked cell))
                , attrIf isActiveCell (class "bg-blue-500")
                ]
                [ span
                    [ attrIf isCorrectValue (class "text-green-500") ]
                    [ viewIf (value /= 0) (text (String.fromInt value)) ]
                ]
        )
        row


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Sudoku-kids"
        [ h1 [ class "py-4 text-4xl text-center" ] [ text "Sudoku-Kids" ]
        , div [ class "w-full text-5xl" ]
            [ div
                [ class "mx-auto bg-blue-200 shadow-lg grid grid-rows-4 grid-cols-4"
                , class "sudoku-grid"
                ]
                (viewPuzzle model)
            , div []
                [ case model.activeCell of
                    Just cell ->
                        viewInputNumbers cell

                    Nothing ->
                        text ""
                ]
            , viewIf (Grid.equals model.puzzle model.solution) <|
                div
                    [ class "flex flex-col items-center mx-auto text-6xl", style "width" "50vw" ]
                    [ div [ class "animate-bounce" ] [ text "\u{1F973}" ]
                    , button [ onClick Restart ] [ text "🔁" ]
                    ]
            ]
        ]
