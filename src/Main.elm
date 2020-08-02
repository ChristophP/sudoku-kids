module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Grid exposing (Grid)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)


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
    { grid : Grid Int }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { grid = Grid.init 3 }, Cmd.none )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


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
                (Grid.toList model.grid
                    |> List.map (\val -> div [ class "flex items-center justify-center border border-gray-800" ] [ text (String.fromInt val) ])
                )
            ]
        ]
