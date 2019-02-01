module Driver exposing (main, hueChanges)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Html exposing (Html)
import Color
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (viewBox, height, width)
import TypedSvg.Types exposing (px)
import Time
import Random
import Quad exposing (Quad, render)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type Msg
    = NoOp
    | Tick Time.Posix
    | GetRandomNumbers (List Float)


type alias Model =
    { count : Int
    , randomNumbers : List Float
    , drawing : List Quad
    , depth : Int
    , maxDepth : Int
    , count : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { count = 0
      , randomNumbers = List.repeat 10 0.1
      , drawing = [ Quad.basic 750 ]
      , depth = 1
      , maxDepth = 6
      , count = 0
      }
    , Cmd.none
    )


hueChange : Float -> List Float
hueChange h =
    [ h, 0, 0, 0 ]


hueSaturationChange : Float -> Float -> List Float
hueSaturationChange h s =
    [ h, s, s, s ]


hueChanges : List Float -> List (List Float)
hueChanges dhList =
    List.map hueChange dhList


hueSaturationChanges : List Float -> List Float -> List (List Float)
hueSaturationChanges dhList dsList =
    List.map2 hueSaturationChange dhList dsList


subscriptions model =
    Time.every 300 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetRandomNumbers r ->
            ( { model | randomNumbers = r }, Cmd.none )

        Tick t ->
            if model.depth < model.maxDepth then
                let
                    rands =
                        Debug.log "rand"
                            (model.randomNumbers
                                |> List.map (\x -> (2 * x - 1) / 8.0)
                            )

                    colorChanges =
                        hueSaturationChanges (List.take 5 rands) (List.drop 5 rands)

                    newDrawing =
                        Quad.update
                            Quad.basicColorRange
                            colorChanges
                            Quad.sampleProportions
                            model.drawing
                in
                    ( { model
                        | depth = model.depth + 1
                        , drawing = newDrawing
                        , count = model.count + 1
                      }
                    , Random.generate GetRandomNumbers (Random.list 10 (Random.float 0 1))
                    )
            else
                ( { model | count = model.count + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    svg
        [ width (px 900), height (px 900) ]
    <|
        List.map (Quad.render Quad.hsla) model.drawing
