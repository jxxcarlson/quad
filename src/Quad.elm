module Quad
    exposing
        ( Quad
        , ColorRange
        , Proportions
        , render
        , update
        , basic
        , sampleProportions
        , basicColorRange
        , hsla
        , rgba
        , addChangesToProportions
        )

{- }(Quad(..), Vertices, basic, vertices, color, subdivide) -}

import Array exposing (Array)
import Maybe.Extra
import Color
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (points, fill, stroke)
import TypedSvg.Types exposing (Fill(..), px)
import Svg exposing (Svg)


type Quad
    = Quad Vertices Color


type alias Point =
    ( Float, Float )


type alias Vertices =
    Array Point


{-| Use an array of size 4 with components
in the range [0,1]. A color can be interpreted
as either rgba or hsla.
-}
type alias Color =
    List Float


type alias ColorRange =
    List ( Float, Float )


type alias ColorChange =
    List Float


type alias ColorMap =
    Quad -> Color.Color


{-| Use an array of size four with components in the
range [a,b] where 0 < a < b < 1. The endpoints should
not be too close to zero or to one. The components of this
vector determine where division points are placed along the
sides of quadrilateral when subdividing it.
-}
type alias Proportions =
    Array Float


render : ColorMap -> Quad -> Svg msg
render colorMap quad =
    TypedSvg.polygon
        [ fill <| Fill (colorMap quad)
        , stroke Color.black
        , points (Array.toList <| vertices quad)
        ]
        []


addChangesToProportions : Float -> Float -> List Float -> Proportions -> Proportions
addChangesToProportions lowerBound upperBound dps proportions =
    let
        proportionList =
            Array.toList proportions

        newProportions =
            List.map2 (+) proportionList dps
                |> List.map (clamp lowerBound upperBound)
    in
        Array.fromList newProportions


{-| update basicColorRange [sampleColorChange] ps [basic 4]
-}
update : ColorRange -> List ColorChange -> Proportions -> List Quad -> List Quad
update colorRange colorChangeList proportions quadList =
    quadList
        |> List.map (subdivide proportions)
        |> List.concat
        |> changeColorOfQuadList colorRange colorChangeList



--
-- DATA
--


basicColorRange =
    [ ( 0.5, 0.6 ), ( 0.2, 0.4 ), ( 0.0, 1.0 ), ( 0.99, 1.0 ) ]


sampleColorChange =
    [ 0.01, -0.02, -0.01, 0.02 ]


sampleColor =
    [ 0.5, 0.5, 0.7, 0.7 ]


sampleProportions =
    Array.fromList [ 0.4, 0.5, 0.3, 0.7 ]


sampleProportions1 =
    Array.fromList [ 0.4, 0.5, 0.6, 0.7 ]


{-| a simple quadrilateral for testing and initializatin
-}
basic : Float -> Quad
basic size =
    let
        a =
            ( 0, 0 )

        b =
            ( size, 0 )

        c =
            ( size, size )

        d =
            ( 0, size )

        p =
            1

        q =
            0

        r =
            0

        s =
            1

        vertices_ =
            Array.fromList [ a, b, c, d ]

        color_ =
            [ p, q, r, s ]
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
-- COLOR
--


hsla : Quad -> Color.Color
hsla (Quad v cc) =
    case cc of
        [ a, b, c, d ] ->
            Color.hsla a b c d

        _ ->
            Color.black


rgba : Quad -> Color.Color
rgba (Quad v cc) =
    case cc of
        [ a, b, c, d ] ->
            Color.rgba a b c d

        _ ->
            Color.black


{-| subdivide ps (basic 4) |> changeColorOfQuadList basicColorRange [sampleColorChange]
-}
changeColorOfQuadList : ColorRange -> List ColorChange -> List Quad -> List Quad
changeColorOfQuadList colorRange colorChangeList quadList =
    let
        colorChangeList_ =
            extendList (List.length quadList) colorChangeList
    in
        List.map2 (\colorChange_ quad_ -> changeColorOfQuad colorRange colorChange_ quad_)
            colorChangeList_
            quadList


extendList : Int -> List a -> List a
extendList n list =
    let
        listLength =
            List.length list

        blocks =
            n // listLength

        remainder =
            modBy listLength n
    in
        (List.repeat blocks list |> List.concat)
            ++ List.take remainder list
            |> List.take n


{-| changeColorOfQuad basicColorRange sampleColorChange (basic 4)
Quad (Array.fromList [(0,0),(4,0),(4,4),(0,4)]) [0.8,0.5,0.5,1]
: Quad
-}
changeColorOfQuad : ColorRange -> ColorChange -> Quad -> Quad
changeColorOfQuad colorRange_ colorChange_ (Quad vertices_ color_) =
    Quad vertices_ (changeColor colorRange_ colorChange_ color_)


changeColor : ColorRange -> ColorChange -> Color -> Color
changeColor colorRange_ colorChange_ color_ =
    let
        clamps =
            List.map (\( a, b ) -> clamp a b) colorRange_
    in
        List.map2 (\f x -> f x)
            clamps
            (List.map2 (+) colorChange_ color_)



--
-- SUBDIVIDE
--


{-| subdivide ps (basic 4) |> List.map (subdivide ps) |> List.concat
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
