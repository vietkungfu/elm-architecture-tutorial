import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char exposing (isDigit, isUpper, isLower)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validation : Validation
  }


model : Model
model =
  Model "" "" "" "" None



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit

type Validation
    = None
    | OK
    | Error String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }

    Submit ->
      {model | validation = validate model}



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [] [ input [ type_ "text", placeholder "Name", onInput Name ] [] ]
    , div [] [ input [ type_ "password", placeholder "Password", onInput Password ] [] ]
    , div [] [ input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] [] ]
    , div [] [ input [ type_ "number", placeholder "Your age", onInput Age ] [] ]
    , div [] [ button [ onClick Submit ] [ text "Submit" ] ]
    , viewValidation model.validation
    ]

validate : Model -> Validation
validate model =
    let
      { name, password, passwordAgain, age, validation} = model
    in
      if password /= passwordAgain then
        Error "Passwords do not match"
      else if String.length password < 8 then
        Error "Passwords must be longer than 8 characters"
      else if not (String.any isUpper password) then
        Error "Password must contain uppercase"
      else if not (String.any isLower password) then
        Error "Password must contain lowercase"
      else if not (String.any isDigit password) then
        Error "Password must contain nummeric characters"
      else if not (String.all isDigit age) then
        Error "Age must be a positive number"
      else
        OK

viewValidation : Validation -> Html msg
viewValidation validation =
  case validation of
    None ->
      div [] [ text "" ]
    OK ->
      div [ style [("color", "green")] ] [ text "OK" ]
    Error err ->
      div [ style [("color", "red")] ] [ text err ]
