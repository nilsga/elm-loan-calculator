module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import LoanCalculations exposing (..)
import Material.Textfield as Textfield
import Material.Options as Options
import Material
import String


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }


type alias Model =
    { loanDetails : LoanDetails
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model


type Msg
    = LoanAmount String
    | LoanInterestRate String
    | LoanDuration String
    | Mdl (Material.Msg Msg)


model : Model
model =
    { loanDetails =
        { amount = 2000000
        , interestRate = 0.025
        , terms = 12
        , duration = 20
        , termFee = 0
        , initialFee = 0
        }
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg oldModel =
    let
        oldLoanDetails =
            oldModel.loanDetails
    in
        case msg of
            LoanAmount newAmount ->
                ( { oldModel | loanDetails = { oldLoanDetails | amount = Result.withDefault 2000000 <| String.toInt newAmount } }, Cmd.none )

            LoanInterestRate newInterestRate ->
                ( { oldModel | loanDetails = { oldLoanDetails | interestRate = Result.withDefault 0.025 <| String.toFloat newInterestRate } }, Cmd.none )

            LoanDuration newDuration ->
                ( { oldModel | loanDetails = { oldLoanDetails | duration = Result.withDefault 20 <| String.toInt newDuration } }, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ oldModel


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Lånebeløp"
                , Options.onInput LoanAmount
                ]
                []
            ]
        , div []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Rente"
                , Options.onInput LoanInterestRate
                ]
                []
            ]
        , div []
            [ Textfield.render Mdl
                [ 2 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Nedbetalingstid"
                , Options.onInput LoanDuration
                ]
                []
            ]
        , div [] [ text (toString (termAmount model.loanDetails)) ]
        ]
