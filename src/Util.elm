module Util exposing (..)

import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as JD


listChunk : Int -> List a -> List (List a)
listChunk size list =
    listChunkHelp size list []


listChunkHelp : Int -> List a -> List (List a) -> List (List a)
listChunkHelp size list chunks =
    let
        newChunks =
            chunks ++ [ List.take size list ]
    in
    case List.drop size list of
        [] ->
            newChunks

        rest ->
            listChunkHelp size rest newChunks


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click" (JD.succeed ( msg, True ))


viewIf : Bool -> Html msg -> Html msg
viewIf show html =
    if show then
        html

    else
        text ""


attrIf : Bool -> Attribute msg -> Attribute msg
attrIf show attr =
    if show then
        attr

    else
        class ""
