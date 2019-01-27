module Quad exposing (Quad(..), Vertices, basic, vertices, color, subdivide, center)

import Array exposing (Array)
import Maybe.Extra


type Quad
    = Quad Vertices Color


type alias Point =
    ( Float, Float )


type alias Vertices =
    Array Point


type alias Color =
    Array Float


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
            1

        gg =
            0

        bb =
            0

        aa =
            1

        vertices_ =
            Array.fromList [ a, b, c, d ]

        color_ =
            Array.fromList [ rr, gg, bb, aa ]
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


{-|

> subdivide ps basic |> List.map (subdivide ps) |> List.concat
-}
subdivide : Proportions -> Quad -> List Quad
subdivide proportions quad =
    let
        oldVertices_ =
            vertices quad

        newVertices_ =
            newVertices proportions quad

        center_ =
            center newVertices_

        vList0 =
            makeVertices 3 0 0 center_ oldVertices_ newVertices_

        vList1 =
            makeVertices 0 1 1 center_ oldVertices_ newVertices_

        vList2 =
            makeVertices 1 2 2 center_ oldVertices_ newVertices_

        vList3 =
            makeVertices 2 3 3 center_ oldVertices_ newVertices_

        colors =
            color quad
    in
        [ vList0, vList1, vList2, vList3 ]
            |> List.map (\vv -> Maybe.map2 Quad vv (Just (color quad)))
            |> Maybe.Extra.combine
            |> Maybe.withDefault []


makeVertices : Int -> Int -> Int -> Maybe Point -> Vertices -> Maybe Vertices -> Maybe Vertices
makeVertices i j k center_ oldVertices_ newVertices_ =
    let
        a =
            Maybe.andThen (Array.get i) newVertices_

        b =
            Array.get j oldVertices_

        c =
            Maybe.andThen (Array.get k) newVertices_
    in
        [ a, b, c, center_ ] |> Maybe.Extra.combine |> Maybe.map Array.fromList


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
        det =
            (b - d)
                * (e - g)
                - (c - a)
                * (h - f)

        p =
            (b - d) * a + (c - a) * b

        q =
            (h - f) * g + (e - g) * h

        xminor =
            (p * (e - g) - q * (c - a))

        yminor =
            (q * (b - d) - p * (h - f))
    in
        Just ( xminor / det, yminor / det )


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
