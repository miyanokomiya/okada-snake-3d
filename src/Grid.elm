module Grid exposing
    ( Grid
    , Line
    , Plane
    , Point
    , get
    , initialize
    , length
    , map
    , repeat
    , set
    , toList
    )

import Array exposing (Array)


type alias Line a =
    Array a


type alias Plane a =
    Array (Line a)


type alias Grid a =
    Array (Plane a)


type alias Point =
    ( Int, Int, Int )


length : Grid a -> Int
length grid =
    Array.length grid


set : Point -> a -> Grid a -> Grid a
set ( x, y, z ) item grid =
    grid
        |> map
            (\( old, ( xx, yy, zz ) ) ->
                if xx == x && yy == y && zz == z then
                    item

                else
                    old
            )


get : Point -> Grid a -> Maybe a
get ( x, y, z ) grid =
    case Array.get z grid of
        Nothing ->
            Nothing

        Just plane ->
            case Array.get y plane of
                Nothing ->
                    Nothing

                Just line ->
                    Array.get x line


toList : Grid a -> List ( Point, a )
toList grid =
    grid
        |> Array.indexedMap
            (\z plane ->
                plane
                    |> Array.indexedMap
                        (\y line ->
                            line
                                |> Array.indexedMap
                                    (\x cell ->
                                        ( ( x, y, z ), cell )
                                    )
                                |> Array.toList
                        )
                    |> Array.toList
                    |> List.concat
            )
        |> Array.toList
        |> List.concat


repeat : Int -> a -> Grid a
repeat size item =
    let
        line =
            Array.repeat size item

        plane =
            Array.repeat size line
    in
    Array.repeat size plane


initialize : Int -> (Point -> a) -> Grid a
initialize size gen =
    Array.initialize size
        (\z ->
            Array.initialize size
                (\y ->
                    Array.initialize size
                        (\x ->
                            gen ( x, y, z )
                        )
                )
        )


map : (( a, ( Int, Int, Int ) ) -> a) -> Grid a -> Grid a
map exec grid =
    grid
        |> Array.indexedMap
            (\z plane ->
                plane
                    |> Array.indexedMap
                        (\y line ->
                            line
                                |> Array.indexedMap
                                    (\x cell ->
                                        exec ( cell, ( x, y, z ) )
                                    )
                        )
            )
