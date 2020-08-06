module Util exposing (..)

import Html exposing (Attribute)
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
