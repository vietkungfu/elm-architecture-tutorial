import Html exposing (Html, div, button)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { time : Time
  , allowTick : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model 0 True, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | SetTickState


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({model | time = newTime}, Cmd.none)
    SetTickState ->
      ({model | allowTick = not(model.allowTick)}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.allowTick of
    True ->
      Time.every second Tick
    False ->
      Sub.none

-- UTILITIES
-- Convert Time to Angle

convertToAngle : Float -> Int -> Float
convertToAngle value oneTurnValue =
  let
    v = truncate value
    remainderValue = rem v oneTurnValue |> toFloat
    turnValue =
      oneTurnValue
      |> toFloat
      |> (/) remainderValue
      |> (+) 0.75
      |> turns
  in
    turnValue


-- VIEW


view : Model -> Html Msg
view model =
  let

    secondAngle = convertToAngle (Time.inSeconds model.time) 60

    secondX =
      toString (50 + 40 * cos secondAngle)

    secondY =
      toString (50 + 40 * sin secondAngle)

    minuteAngle = convertToAngle (Time.inMinutes model.time) 60

    minuteX =
      toString (50 + 35 * cos minuteAngle)

    minuteY =
      toString (50 + 35 * sin minuteAngle)

    hourAngle = convertToAngle (Time.inHours model.time) 12

    hourX =
      toString (50 + 30 * cos hourAngle)

    hourY =
      toString (50 + 30 * sin hourAngle)
  in
    div []
      [ div []
          [svg [ viewBox "0 0 100 100", width "300px" ]
            [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
            , line [ x1 "50", y1 "50", x2 secondX, y2 secondY, stroke "#023963" ] []
            , line [ x1 "50", y1 "50", x2 minuteX, y2 minuteY, stroke "#008000" ] []
            , line [ x1 "50", y1 "50", x2 hourX, y2 hourY, stroke "#FF0000" ] []
            ]
          ]
      , div [] [ button [onClick SetTickState] [ text "CHANGE CLOCK TICK STATE"]]
      ]
