module Parameters exposing (Parameter)

import Json.Decode as D exposing (Decoder)
import Quad exposing (ColorRange, Proportions)


type alias Parameter =
    { colorRange : ColorRange
    , proportions : Proportions
    , maxDepth : Int
    }


type alias Range =
    { lo : Float, hi : Float }


parameterDecoder : Decoder Parameter
parameterDecoder =
    D.map3 Parameter
        (D.field "colorRange" colorRangeDecoder)
        (D.field "proportions" proportionsDecoder)
        (D.field "maxDepth" D.int)


proportionsDecoder : Decoder Proportions
proportionsDecoder =
    D.array D.float


colorRangeDecoder : Decoder ColorRange
colorRangeDecoder =
    D.list (rangeDecoder |> D.map (\r -> ( r.lo, r.hi )))


rangeDecoder : Decoder Range
rangeDecoder =
    D.map2 Range
        (D.field "lo" D.float)
        (D.field "hi" D.float)
