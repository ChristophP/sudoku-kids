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



-- trickier than you think
-- in certain cases some values need to be excluded, otherwise the sudoku might end up being invalid
-- explanation: rows in quadrant 3 should not occur in as cols in quadrant two (order does not matter)
--    otherwise solving the puzzle is not possible


generateThirdBox : Random.Seed -> Grid Int -> ( Grid Int, Seed )
generateThirdBox seed grid =
    let
        preventInvalidSudoku num =
            -- find in same column as num in second quadrant and exclude it
            case Grid.getBox Grid.BR grid of
                [ b11, b12, b21, b22 ] ->
                    if num == b11 then
                        b21

                    else if num == b12 then
                        b22

                    else if num == b21 then
                        b11

                    else
                        b12

                -- should not happen
                _ ->
                    -1

        num31 =
            tryNumber (makeNumbersToTry []) (Grid.getCross Grid.G31 grid)

        excludeNum =
            preventInvalidSudoku num31

        num32 =
            tryNumber (makeNumbersToTry [ num31, excludeNum ]) (Grid.getCross Grid.G32 grid)

        num41 =
            tryNumber (makeNumbersToTry [ num31, num32 ]) (Grid.getCross Grid.G41 grid)

        num42 =
            tryNumber (makeNumbersToTry [ num31, num32, num41 ]) (Grid.getCross Grid.G42 grid)

        newGrid =
            Grid.set Grid.G31 num31 grid
                |> Grid.set Grid.G32 num32
                |> Grid.set Grid.G41 num41
                |> Grid.set Grid.G42 num42
    in
    ( newGrid, seed )


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
