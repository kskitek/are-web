module Main exposing (Field(..), Model, Msg(..), Token, decodeToken, errorCodeToMessage, handleLoginResult, initModel, login, main, subscriptions, update, updateField, view, viewValidation)

import Browser
import Debug
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Maybe


main =
    Browser.element { init = initModel, subscriptions = subscriptions, update = update, view = view }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { username : String
    , password : String
    , token : Maybe String
    , error : Maybe Http.Error
    }


type alias Token =
    { token : String
    }


type Msg
    = Login
    | SetField Field String
    | LoginResult (Result Http.Error Token)


type Field
    = UserName
    | Password


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( { username = ""
      , password = ""
      , token = Nothing
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetField field value ->
            updateField field model value

        Login ->
            login model

        LoginResult result ->
            ( handleLoginResult model result, Cmd.none )


updateField : Field -> Model -> String -> ( Model, Cmd Msg )
updateField field model value =
    case field of
        UserName ->
            ( { model | username = value }, Cmd.none )

        Password ->
            ( { model | password = value }, Cmd.none )


login : Model -> ( Model, Cmd Msg )
login model =
    let
        x =
            Debug.log "logging in" model

        address =
            "http://localhost:8080/login"

        payload =
            Http.stringBody "application/json" """{"name" : "101", "password" : "102"}"""

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = address
                , body = payload
                , expect = Http.expectJson decodeToken
                , timeout = Nothing
                , withCredentials = False
                }
    in
    ( model, Http.send LoginResult request )


handleLoginResult : Model -> Result Http.Error Token -> Model
handleLoginResult model result =
    let
        _ =
            Debug.log "loginResult" (Debug.toString result)

        ( token, error ) =
            case result of
                Err err ->
                    ( Nothing, Just err )

                Ok payload ->
                    ( Just payload.token, Nothing )
    in
    { model | token = token, error = error }


decodeToken : Json.Decode.Decoder Token
decodeToken =
    Json.Decode.map Token
        (Json.Decode.field "token" Json.Decode.string)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "login-input" ]
            [ input [ onInput <| SetField UserName, type_ "text", placeholder "Username" ] []
            , input [ onInput <| SetField Password, type_ "password", placeholder "Password" ] []
            , button [ onClick Login ] [ text "Login" ]
            , viewValidation model
            ]
        ]


viewValidation : Model -> Html Msg
viewValidation model =
    let
        _ =
            model
                |> Debug.toString
                |> Debug.log "validation"

        wasError =
            model.error /= Nothing

        message =
            Maybe.map errorCodeToMessage model.error |> Maybe.withDefault "Unable to log in"
    in
    div [ Html.Attributes.classList [ ( "validation", True ), ( "invalid", wasError ) ] ] [ text message ]


errorCodeToMessage : Http.Error -> String
errorCodeToMessage error =
    let
        defaultMsg =
            "Unable to log in"
    in
    case error of
        Http.BadStatus response ->
            case response.status.code of
                403 ->
                    "Invalid username or password"

                _ ->
                    defaultMsg

        _ ->
            defaultMsg
