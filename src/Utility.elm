module Utility exposing (..)


roundToPlaces : Int -> Float -> Float
roundToPlaces k x =
    let
        f =
            toFloat <| 10 ^ k
    in
        (toFloat (round <| f * x)) / f


mapToRange : Float -> Float -> Float -> Float -> Float -> Float
mapToRange a b c d x =
    let
        k =
            (d - c) / (b - a)

        xx =
            clamp a b x
    in
        k * (xx - a) + c
