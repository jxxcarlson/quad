module Quad exposing (..)

import Array exposing (Array)


foo =
    1


type Quad
    = Quad Vertices Color


type alias Point =
    ( Float, Float )


type alias Vertices =
    Array Point


type alias Color =
    Array Int


type alias Proportions =
    Array Float


type alias ColorMap =
    Color -> Color


basic : Quad
basic =
    let
        a =
            ( 0, 0 )

        b =
            ( 1, 0 )

        c =
            ( 1, 1 )

        d =
            ( 0, 1 )

        rr =
            255

        gg =
            0

        bb =
            0

        vertices_ =
            Array.fromList [ a, b, c, d ]

        color_ =
            Array.fromList [ rr, gg, bb ]
    in
        Quad vertices_ color_



--
-- ACCESSORS
--


vertices : Quad -> Vertices
vertices (Quad vertices_ color_) =
    vertices_


color : Quad -> Color
color (Quad vertices_ color_) =
    color_



--
-- SUBDIVIDE
--
--


subdivide : Proportions -> ColorMap -> Quad -> List Quad
subdivide proportions colorMap quad =
    [ quad ]


newVertices : Proportions -> Quad -> Vertices
newVertices proportions quad =
    let
        oldVertices =
            vertices quad
    in
        oldVertices


{-|

> divisionPoint 0.5 (0,0) (1,1)
> (0.5,0.5) : Point
-}
divisionPoint : Float -> Point -> Point -> Point
divisionPoint p a b =
    let
        x =
            p * (Tuple.first a) + (1 - p) * (Tuple.first b)

        y =
            p * (Tuple.second a) + (1 - p) * (Tuple.second b)
    in
        ( x, y )
