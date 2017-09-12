import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFace1 : Int,
    dieFace2 : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace1 Int
  | NewFace2 Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace1 (Random.int 1 6))

    NewFace1 newFace ->
      ({model | dieFace1 = newFace }, Random.generate NewFace2 (Random.int 1 6))

    NewFace2 newFace ->
      ({model | dieFace2 = newFace }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

imageUrl : Int -> String
imageUrl num =
  "./04-random/" ++ toString num ++ ".png"


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ img [src (imageUrl model.dieFace1)] []
    , h1 [] [ text (toString model.dieFace1) ]
    , img [src (imageUrl model.dieFace2)] []
    , h1 [] [ text (toString model.dieFace2) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
