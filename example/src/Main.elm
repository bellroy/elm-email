module Main exposing (main)

import Browser exposing (element)
import Email
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { userInput : String }


type Msg
    = OnUserInputEmail String


main : Program () Model Msg
main =
    element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { userInput = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUserInputEmail string ->
            ( { userInput = string }, Cmd.none )


view : Model -> Html.Html Msg
view { userInput } =
    let
        email =
            Email.fromString userInput

        isValid =
            case email of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    Html.form []
        [ div [ class "form-group" ]
            [ label [ for "userInput" ] [ text "email" ]
            , input
                [ name "userInput"
                , classList
                    [ ( "form-control", True )
                    , ( "is-invalid", (String.length userInput > 1) && not isValid )
                    , ( "is-valid", (String.length userInput > 1) && isValid )
                    ]
                , type_ "text"
                , onInput OnUserInputEmail
                , value userInput
                ]
                []
            ]
        , pre [] [ code [] [ text "Email.fromString: String -> Email" ] ]
        , case email of
            Nothing ->
                pre [] [ code [] [ text "Nothing" ] ]

            Just emailRecord ->
                pre []
                    [ code []
                        [ text "Just ("
                        , text <| Debug.toString emailRecord
                        , text ")"
                        ]
                    ]
        ]
