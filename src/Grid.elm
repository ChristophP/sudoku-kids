module Grid exposing (..)


type Grid a
    = Grid
        { g11 : a
        , g12 : a
        , g13 : a
        , g14 : a
        , g21 : a
        , g22 : a
        , g23 : a
        , g24 : a
        , g31 : a
        , g32 : a
        , g33 : a
        , g34 : a
        , g41 : a
        , g42 : a
        , g43 : a
        , g44 : a
        }


type Line
    = L1
    | L2
    | L3
    | L4


type Cell
    = G11
    | G12
    | G13
    | G14
    | G21
    | G22
    | G23
    | G24
    | G31
    | G32
    | G33
    | G34
    | G41
    | G42
    | G43
    | G44


get : Cell -> Grid a -> a
get cell (Grid grid) =
    case cell of
        G11 ->
            grid.g11

        G12 ->
            grid.g12

        G13 ->
            grid.g13

        G14 ->
            grid.g14

        G21 ->
            grid.g21

        G22 ->
            grid.g22

        G23 ->
            grid.g23

        G24 ->
            grid.g24

        G31 ->
            grid.g31

        G32 ->
            grid.g32

        G33 ->
            grid.g33

        G34 ->
            grid.g34

        G41 ->
            grid.g41

        G42 ->
            grid.g42

        G43 ->
            grid.g43

        G44 ->
            grid.g44


set : Cell -> a -> Grid a -> Grid a
set cell val (Grid grid) =
    case cell of
        G11 ->
            Grid { grid | g11 = val }

        G12 ->
            Grid { grid | g12 = val }

        G13 ->
            Grid { grid | g13 = val }

        G14 ->
            Grid { grid | g14 = val }

        G21 ->
            Grid { grid | g21 = val }

        G22 ->
            Grid { grid | g22 = val }

        G23 ->
            Grid { grid | g23 = val }

        G24 ->
            Grid { grid | g24 = val }

        G31 ->
            Grid { grid | g31 = val }

        G32 ->
            Grid { grid | g32 = val }

        G33 ->
            Grid { grid | g33 = val }

        G34 ->
            Grid { grid | g34 = val }

        G41 ->
            Grid { grid | g41 = val }

        G42 ->
            Grid { grid | g42 = val }

        G43 ->
            Grid { grid | g43 = val }

        G44 ->
            Grid { grid | g44 = val }


init : a -> Grid a
init value =
    Grid
        { g11 = value
        , g12 = value
        , g13 = value
        , g14 = value
        , g21 = value
        , g22 = value
        , g23 = value
        , g24 = value
        , g31 = value
        , g32 = value
        , g33 = value
        , g34 = value
        , g41 = value
        , g42 = value
        , g43 = value
        , g44 = value
        }


toList : Grid a -> List a
toList (Grid grid) =
    [ grid.g11
    , grid.g12
    , grid.g13
    , grid.g14
    , grid.g21
    , grid.g22
    , grid.g23
    , grid.g24
    , grid.g31
    , grid.g32
    , grid.g33
    , grid.g34
    , grid.g41
    , grid.g42
    , grid.g43
    , grid.g44
    ]


getRow : Line -> Grid a -> List a
getRow line (Grid grid) =
    case line of
        L1 ->
            [ grid.g11, grid.g12, grid.g13, grid.g14 ]

        L2 ->
            [ grid.g21, grid.g22, grid.g23, grid.g24 ]

        L3 ->
            [ grid.g31, grid.g32, grid.g33, grid.g34 ]

        L4 ->
            [ grid.g41, grid.g42, grid.g43, grid.g44 ]


getCol : Line -> Grid a -> List a
getCol line (Grid grid) =
    case line of
        L1 ->
            [ grid.g11, grid.g21, grid.g31, grid.g41 ]

        L2 ->
            [ grid.g12, grid.g22, grid.g32, grid.g42 ]

        L3 ->
            [ grid.g13, grid.g23, grid.g33, grid.g43 ]

        L4 ->
            [ grid.g14, grid.g24, grid.g34, grid.g44 ]



-- boxes are defined as follows


type BoxPosition
    = TR
    | TL
    | BR
    | BL


getBox : BoxPosition -> Grid a -> List a
getBox line (Grid grid) =
    case line of
        BL ->
            [ grid.g11, grid.g12, grid.g21, grid.g22 ]

        BR ->
            [ grid.g13, grid.g14, grid.g23, grid.g24 ]

        TL ->
            [ grid.g31, grid.g32, grid.g41, grid.g42 ]

        TR ->
            [ grid.g33, grid.g34, grid.g43, grid.g44 ]



-- constraints
-- generate
