module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Grid exposing (Grid)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)
import Sudoku
import Task
import Time


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
    , board : Grid (Maybe Int)
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { solution = Grid.init 0
      , board = Grid.init Nothing
      }
    , Task.perform GotTime Time.now
    )



-- Update


type Msg
    = GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime now ->
            ( { model | solution = Sudoku.generate (Time.posixToMillis now) }
            , Cmd.none
            )



-- VIEW


convertVal val =
    case val of
        Just v ->
            String.fromInt v

        Nothing ->
            ""


view : Model -> Browser.Document msg
view model =
    Browser.Document "Sudoku-kids"
        [ h1 [ class "py-4 text-4xl text-center" ] [ text "Sudoku-Kids" ]
        , div [ class "w-full" ]
            [ div
                [ class "w-full mx-auto text-lg text-5xl bg-blue-200 grid grid-rows-4 grid-cols-4"
                , style "width" "50vw"
                , style "height" "50vw"
                ]
                (Grid.toList model.solution
                    |> List.map (\val -> div [ class "flex items-center justify-center border border-gray-800" ] [ text (String.fromInt val) ])
                )

            --(Grid.toList model.board
            --|> List.map (convertVal >> (\val -> div [ class "flex items-center justify-center border border-gray-800" ] [ text val ]))
            --)
            ]
        ]
