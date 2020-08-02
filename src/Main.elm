module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, h1, text)
import Html.Attributes exposing (class)


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
    {}


init : () -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )



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
    Browser.Document "Give me a title"
        [ h1 [ class "text-4xl py-4 text-center" ] [ text "Let's go pet stack." ] ]
