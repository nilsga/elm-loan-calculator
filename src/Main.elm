module Main exposing (..)

import Html exposing (..)
import LoanCalculations exposing (..)
import Material.Textfield as Textfield
import Material.Options as Options exposing (css)
import Material.Card as Card
import Material.Color as Color
import Material.Typography as Typography
import Material.Button as Button
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material
import String


white =
    Color.text Color.white


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
    , calculatedTermAmount : Float
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model


type Msg
    = LoanAmount String
    | LoanInterestRate String
    | LoanDuration String
    | Recalculate
    | Mdl (Material.Msg Msg)


model : Model
model =
    { loanDetails =
        { amount = 2000000
        , interestRate = 3
        , terms = 12
        , duration = 20
        , termFee = 0
        , initialFee = 0
        }
    , calculatedTermAmount = 0
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
                ( { oldModel | loanDetails = { oldLoanDetails | interestRate = (Result.withDefault 3 <| String.toFloat newInterestRate) / 100 } }, Cmd.none )

            LoanDuration newDuration ->
                ( { oldModel | loanDetails = { oldLoanDetails | duration = Result.withDefault 20 <| String.toInt newDuration } }, Cmd.none )

            Recalculate ->
                ( { oldModel | calculatedTermAmount = (termAmount oldLoanDetails) }, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ oldModel


textField : String -> Int -> Mdl -> (String -> Msg) -> Html Msg
textField name index mdl msg =
    Textfield.render Mdl
        [ index ]
        mdl
        [ Textfield.floatingLabel
        , Textfield.label name
        , Options.onInput msg
        , Options.onBlur Recalculate
        ]
        []


view : Model -> Html Msg
view model =
    Card.view
        [ css "width" "400px"
        , Elevation.e6
        ]
        [ Card.title [ Color.background (Color.color Color.LightBlue Color.S400) ] [ Card.head [ white ] [ text "Lånedetaljer" ] ]
        , Card.text []
            [ textField "Lånbebeløp" 0 model.mdl LoanAmount
            , textField "Nedbetalingstid" 1 model.mdl LoanDuration
            , textField "Rente" 2 model.mdl LoanInterestRate
            ]
          -- Filler
        , Card.actions
            [ Card.border
              -- Modify flexbox to accomodate small text in action block
            , css "display" "flex"
            , css "justify-content" "space-between"
            , css "align-items" "center"
            , css "padding" "8px 16px 8px 16px"
            , Color.background (Color.color Color.LightBlue Color.S400)
            , white
            ]
            [ Options.span [ Typography.caption, Typography.contrast 0.87 ] [ text (toString <| model.calculatedTermAmount) ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.icon, Button.ripple ]
                [ Icon.i "phone" ]
            ]
        ]
