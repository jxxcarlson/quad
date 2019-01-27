module Quad exposing (..)

import Array exposing (Array)
import Maybe.Extra


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
    let
        a =
            1
    in
        [ quad ]


newVertices : Proportions -> Quad -> Maybe Vertices
newVertices proportions quad =
    [ 0, 1, 2, 3 ]
        |> List.map (\k -> divisionPointFromQuad k proportions quad)
        |> Maybe.Extra.combine
        |> Maybe.map Array.fromList


center : Maybe Vertices -> Maybe Point
center vertices_ =
    let
        p =
            Maybe.andThen (Array.get 0) vertices_

        q =
            Maybe.andThen (Array.get 2) vertices_

        r =
            Maybe.andThen (Array.get 3) vertices_

        s =
            Maybe.andThen (Array.get 1) vertices_
    in
        case ( ( p, q ), ( r, s ) ) of
            ( ( Just ( a, b ), Just ( c, d ) ), ( Just ( e, f ), Just ( g, h ) ) ) ->
                computeCenter a b c d e f g h

            ( _, _ ) ->
                Nothing


computeCenter : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Maybe Point
computeCenter a b c d e f g h =
    let
        _ =
            Debug.log "coeff" [ a, b, c, d, e, f, g, h ]

        det =
            (b - d) * (e - g) - (c - a) * (h - f)

        p =
            (b - d) * a + b

        q =
            (h - f) * g + h

        xminor =
            p * (e - g) - q * (c - a)

        yminor =
            q * (b - d) - p * (h - f)
    in
        Just ( xminor / det, -yminor / det )


computeCenter1 : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Maybe Point
computeCenter1 a b c d e f g h =
    let
        m =
            (d - b) / (c - a)

        mm =
            (f - h) / (e - g)

        x =
            (h - b) / (m - mm)

        y =
            m * (x - a) + b
    in
        Just ( x, y )


{-| ps = Array.fromList (List.repeat 4 0.5)
Array.fromList [0.5,0.5,0.5,0.5]

> divisionPointFromQuad 0 ps basic
> Just (0.5,0) : Maybe Point

-}
divisionPointFromQuad : Int -> Proportions -> Quad -> Maybe Point
divisionPointFromQuad k proportions quad =
    let
        p =
            Array.get (modBy 4 k) proportions

        a =
            Array.get (modBy 4 k) (vertices quad)

        b =
            Array.get (modBy 4 (k + 1)) (vertices quad)
    in
        Maybe.map3 divisionPoint p a b


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
