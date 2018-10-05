import Browser
import Html exposing (Html, button, input, div, text)
import Html.Attributes exposing (type_, placeholder)
import Html.Events exposing (onClick, onInput)
import Debug

main =
  Browser.sandbox { init = initModel, update = update, view = view }

type alias Model = {
    username : String
    , password : String
    }

type Msg = Login
    | SetField Field String

type Field = UserName
    | Password

initModel : Model
initModel = {
    username = ""
    , password = ""
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetField field value ->
        updateField field model value
    Login ->
        login model

view : Model -> Html Msg
view model =
  div []
    [ input [ onInput <| SetField UserName, type_ "text", placeholder "Username" ] []
    , input [ onInput <| SetField Password , type_ "password", placeholder "Password" ] []
    , button [ onClick Login ] [ text "Login" ]
    , viewValidation model
    ]

viewValidation : Model -> Html Msg
viewValidation model =
    let
        hidden = False
    in
    div [ Html.Attributes.hidden hidden, Html.Attributes.class "validation" ] [ text "Invalid input" ]

updateField : Field -> Model -> String -> Model
updateField field model value =
    case field of 
        UserName ->
            { model | username = value }
        Password ->
            { model | password = value }

login : model -> model
login model =
    Debug.log "model" model