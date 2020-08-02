module Sudoku exposing (..)

import Grid exposing (Grid)
import Random exposing (Generator, Seed)
import Random.List



-- generate


generate : Seed -> Grid Int
generate seed =
    let
        ( grid1, seed1 ) =
            generateFirstRow seed (Grid.init 0)

        ( grid2, seed2 ) =
            generateSecondRow seed1 grid1

        ( grid3, _ ) =
            generateThirdBox seed2 grid2
    in
    fillLastBox grid3


randomRowGenerator : Generator (List Int)
randomRowGenerator =
    Random.List.shuffle [ 1, 2, 3, 4 ]


generateFirstRow : Random.Seed -> Grid Int -> ( Grid Int, Seed )
generateFirstRow seed grid =
    let
        ( row, newSeed ) =
            Random.step randomRowGenerator seed
    in
    case row of
        [ first, second, third, fourth ] ->
            let
                newGrid =
                    Grid.set Grid.G11 first grid
                        |> Grid.set Grid.G12 second
                        |> Grid.set Grid.G13 third
                        |> Grid.set Grid.G14 fourth
            in
            ( newGrid, newSeed )

        -- should not happen
        _ ->
            ( grid, newSeed )


generateSecondRow : Random.Seed -> Grid Int -> ( Grid Int, Seed )
generateSecondRow seed grid =
    let
        firstRow =
            Grid.getRow Grid.L1 grid

        ( frontPart, seed1 ) =
            Random.step (Random.List.shuffle (List.drop 2 firstRow)) seed

        ( backPart, seed2 ) =
            Random.step (Random.List.shuffle (List.take 2 firstRow)) seed1
    in
    case frontPart ++ backPart of
        [ first, second, third, fourth ] ->
            let
                newGrid =
                    Grid.set Grid.G21 first grid
                        |> Grid.set Grid.G22 second
                        |> Grid.set Grid.G23 third
                        |> Grid.set Grid.G24 fourth
            in
            ( newGrid, seed2 )

        -- should not happen
        _ ->
            ( grid, seed2 )


generateThirdBox : Random.Seed -> Grid Int -> ( Grid Int, Seed )
generateThirdBox seed grid =
    let
        ( leftPart, seed1 ) =
            Random.step (Random.List.shuffle [ Grid.get Grid.G12 grid, Grid.get Grid.G22 grid ]) seed

        ( rightPart, seed2 ) =
            Random.step (Random.List.shuffle [ Grid.get Grid.G11 grid, Grid.get Grid.G21 grid ]) seed1
    in
    case leftPart ++ rightPart of
        [ first, second, third, fourth ] ->
            let
                newGrid =
                    Grid.set Grid.G31 first grid
                        |> Grid.set Grid.G41 second
                        |> Grid.set Grid.G32 third
                        |> Grid.set Grid.G42 fourth
            in
            ( newGrid, seed2 )

        -- should not happen
        _ ->
            ( grid, seed2 )


tryNumber : List Int -> List Int -> Int
tryNumber numbersToTry cross =
    case Debug.log "trying" numbersToTry of
        num :: rest ->
            if List.member num cross then
                tryNumber rest cross

            else
                Debug.log "found" num

        [] ->
            -1


makeNumbersToTry : List Int -> List Int
makeNumbersToTry excludeVals =
    List.filter (\val -> not (List.member val excludeVals)) [ 1, 2, 3, 4 ]



-- implementation could be more efficient by memorizing which numbers were already tried but
-- saving a bunch of milli or microseconds does not matter here


fillLastBox : Grid Int -> Grid Int
fillLastBox grid =
    let
        num33 =
            tryNumber (makeNumbersToTry []) (Grid.getCross Grid.G33 grid)

        num34 =
            tryNumber (makeNumbersToTry [ num33 ]) (Grid.getCross Grid.G34 grid)

        num43 =
            tryNumber (makeNumbersToTry [ num33, num34 ]) (Grid.getCross Grid.G43 grid)

        num44 =
            tryNumber (makeNumbersToTry [ num33, num34, num43 ]) (Grid.getCross Grid.G44 grid)
    in
    Grid.set Grid.G33 num33 grid
        |> Grid.set Grid.G34 num34
        |> Grid.set Grid.G43 num43
        |> Grid.set Grid.G44 num44
