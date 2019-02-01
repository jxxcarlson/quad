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
import Http
import Quad exposing (Quad, Proportions, ColorRange, render)
import Parameters exposing (..)
import Utility


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
    | GotSensorValue (Result Http.Error String)
    | SentLedCommand (Result Http.Error ())


type alias Model =
    { count : Int
    , randomNumbers : List Float
    , drawing : List Quad
    , oldDrawing : List Quad
    , depth : Int
    , proportions : Proportions
    , colorRange : ColorRange
    , maxDepth : Int
    , sensorValue : Maybe Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { count = 0
      , randomNumbers = List.repeat 10 0.1
      , drawing = [ Quad.basic 750 ]
      , oldDrawing = [ Quad.basic 750 ]
      , proportions = Quad.sampleProportions
      , colorRange = [ ( 0.5, 0.6 ), ( 0.5, 0.6 ), ( 0.3, 0.4 ), ( 0.99, 1.0 ) ]
      , depth = 1
      , maxDepth = 6
      , sensorValue = Nothing
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
    Time.every 200 Tick


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
                        model.randomNumbers
                            |> List.map (\x -> (2 * x - 1) / 8.0)

                    colorChanges =
                        hueSaturationChanges (List.take 5 rands) (List.drop 5 rands)

                    newDrawing =
                        Quad.update
                            model.colorRange
                            colorChanges
                            model.proportions
                            model.drawing
                in
                    ( { model | depth = model.depth + 1, drawing = newDrawing, oldDrawing = model.drawing }
                    , Cmd.batch
                        [ Random.generate GetRandomNumbers (Random.list 10 (Random.float 0 1))
                        , ledCommand model.count
                        ]
                    )
            else
                ( model, getSensorValue )

        SentLedCommand result ->
            ( { model | count = model.count + 1 }, Cmd.none )

        GotSensorValue result ->
            case result of
                Ok str ->
                    let
                        maybeSensorValue =
                            String.toFloat str
                                |> Maybe.map (Utility.roundToPlaces 1)
                                |> Maybe.map (Utility.mapToRange 1 50 0 1)

                        newDepth =
                            resetDepth maybeSensorValue model

                        newDrawing =
                            if newDepth == 1 then
                                [ Quad.basic 750 ]
                            else
                                model.drawing
                    in
                        ( { model
                            | sensorValue = maybeSensorValue
                            , colorRange = setColorRange maybeSensorValue model.colorRange
                            , depth = resetDepth maybeSensorValue model
                            , drawing = newDrawing
                          }
                        , Cmd.none
                        )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        currentDrawing =
            if model.depth == 1 then
                model.oldDrawing
            else
                model.drawing
    in
        svg
            [ width (px 900), height (px 900) ]
        <|
            List.map (Quad.render Quad.hsla) currentDrawing


getSensorValue : Cmd Msg
getSensorValue =
    Http.get
        { url = "http:raspberrypi.local:8000/distance"
        , expect = Http.expectString GotSensorValue
        }


ledCommand : Int -> Cmd Msg
ledCommand count =
    case modBy 2 count == 0 of
        True ->
            ledOn

        False ->
            ledOff


ledOn : Cmd Msg
ledOn =
    Http.get
        { url = "http:raspberrypi.local:8000/ledOn"
        , expect = Http.expectWhatever SentLedCommand
        }


ledOff : Cmd Msg
ledOff =
    Http.get
        { url = "http:raspberrypi.local:8000/ledOff"
        , expect = Http.expectWhatever SentLedCommand
        }


setColorRange : Maybe Float -> ColorRange -> ColorRange
setColorRange sensorValue colorRange =
    case sensorValue of
        Nothing ->
            colorRange

        Just p ->
            ( p / 2, p ) :: (List.drop 1 colorRange)


resetDepth : Maybe Float -> Model -> Int
resetDepth maybeSensorValue model =
    case ( maybeSensorValue, model.sensorValue ) of
        ( Nothing, _ ) ->
            model.depth

        ( Just newSensorValue, Nothing ) ->
            1

        ( Just newSensorValue, Just oldSensorValue ) ->
            case abs (newSensorValue - oldSensorValue) < 0.05 of
                True ->
                    model.depth

                False ->
                    if model.depth == model.maxDepth then
                        1
                    else
                        model.depth
