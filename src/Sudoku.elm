module Sudoku exposing (..)

import Grid exposing (Grid)
import Random
import Random.List



-- generate


generate : Int -> Grid Int
generate intSeed =
    let
        seed =
            Random.initialSeed intSeed

        grid =
            Grid.init 0

        generator =
            Random.List.shuffle [ 1, 2, 3, 4 ]

        ( row, _ ) =
            Random.step generator seed
    in
    case row of
        [ first, second, third, fourth ] ->
            Grid.set Grid.G11 first grid
                |> Grid.set Grid.G12 second
                |> Grid.set Grid.G13 third
                |> Grid.set Grid.G14 fourth

        -- should not happen
        _ ->
            grid
