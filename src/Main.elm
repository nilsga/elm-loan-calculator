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
import FormatNumber exposing (format)
import Locales exposing (Locale)
import String
import Char
import String.Extra exposing (replace)
import Task


white =
    Color.text Color.white


intLocale : Locale
intLocale =
    Locale 0 "." ","


floatLocale : Locale
floatLocale =
    Locale 2 "." ","


main : Program Never Model Msg
main =
    program
        { init = update Recalculate model
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }


type alias InputModel =
    { amount : String
    , duration : String
    , interestRate : String
    }


type alias Model =
    { loanDetails : LoanDetails
    , inputModel : InputModel
    , calculatedTermAmount : Float
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model


type RawInput
    = LoanAmountRaw String
    | LoanInterestRateRaw String
    | LoanDurationRaw String


type ConvertedInput
    = LoanAmount Float
    | LoanDuration Float
    | LoanInterestRate Float


type Msg
    = RawWrapper RawInput
    | ConvertedWrapper ConvertedInput
    | Recalculate
    | ParseError String
    | Mdl (Material.Msg Msg)


model : Model
model =
    { loanDetails =
        { amount = 2000000
        , interestRate = 0.03
        , terms = 12
        , duration = 20
        , termFee = 0
        , initialFee = 0
        }
    , inputModel =
        { amount = "2000000"
        , duration = "20"
        , interestRate = "3"
        }
    , calculatedTermAmount = 0
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg oldModel =
    let
        oldLoanDetails =
            oldModel.loanDetails

        oldInputModel =
            oldModel.inputModel
    in
        case msg of
            RawWrapper msg_ ->
                processInput msg_ oldModel

            ConvertedWrapper msg_ ->
                processUpdate msg_ oldModel

            Recalculate ->
                ( { oldModel | calculatedTermAmount = (termAmount oldLoanDetails) }, Cmd.none )

            ParseError err ->
                ( oldModel, Cmd.none )

            Mdl msg_ ->
                Material.update Mdl msg_ oldModel


processInput : RawInput -> Model -> ( Model, Cmd Msg )
processInput msg model =
    let
        handleResult : Result String Float -> (Float -> ConvertedInput) -> Msg
        handleResult result ctor =
            case result of
                Ok value ->
                    ConvertedWrapper (ctor value)

                Err str ->
                    ParseError str

        filterStr : String -> String
        filterStr str =
            replace "," "." <| String.filter (\c -> Char.isDigit c || c == ',') str

        inputs =
            model.inputModel
    in
        case msg of
            LoanAmountRaw value ->
                ( { model | inputModel = { inputs | amount = value } }, Task.attempt (\res -> handleResult res LoanAmount) (toTask <| (String.toFloat (filterStr value))) )

            LoanDurationRaw value ->
                ( { model | inputModel = { inputs | duration = value } }, Task.attempt (\res -> handleResult res LoanDuration) (toTask <| (String.toFloat (filterStr value))) )

            LoanInterestRateRaw value ->
                ( { model | inputModel = { inputs | interestRate = value } }, Task.attempt (\res -> handleResult res LoanInterestRate) (toTask <| (String.toFloat (filterStr value))) )


processUpdate : ConvertedInput -> Model -> ( Model, Cmd Msg )
processUpdate msg model =
    let
        values =
            model.loanDetails
    in
        case msg of
            LoanAmount amount ->
                ( { model | loanDetails = { values | amount = round amount } }, Cmd.none )

            LoanDuration duration ->
                ( { model | loanDetails = { values | duration = round duration } }, Cmd.none )

            LoanInterestRate interestRate ->
                ( { model | loanDetails = { values | interestRate = interestRate / 100 } }, Cmd.none )


toTask : Result String Float -> Task.Task String Float
toTask result =
    case result of
        Ok val ->
            Task.succeed val

        Err str ->
            Task.fail str


textField : String -> Int -> Mdl -> String -> (String -> Msg) -> Html Msg
textField name index mdl value msg =
    Textfield.render Mdl
        [ index ]
        mdl
        [ Textfield.floatingLabel
        , Textfield.value value
        , Textfield.label name
        , Options.onInput msg
        , Options.onBlur Recalculate
        ]
        []


display : String -> Locale -> String
display str locale =
    Result.withDefault str (Result.map (format intLocale) (String.toFloat (String.filter (\c -> c /= '.') str)))


view : Model -> Html Msg
view model =
    Card.view
        [ css "width" "400px"
        , Elevation.e6
        ]
        [ Card.title [ Color.background (Color.color Color.LightBlue Color.S400) ] [ Card.head [ white ] [ text "Lånedetaljer" ] ]
        , Card.text []
            [ textField "Lånebeløp" 0 model.mdl (display model.inputModel.amount intLocale) (\str -> RawWrapper (LoanAmountRaw str))
            , textField "Nedbetalingstid" 1 model.mdl model.inputModel.duration (\str -> RawWrapper (LoanDurationRaw str))
            , textField "Rente" 2 model.mdl model.inputModel.interestRate (\str -> RawWrapper (LoanInterestRateRaw str))
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
            [ Options.span [ Typography.caption, Typography.contrast 0.87 ] [ text (toString <| (round model.calculatedTermAmount)) ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.icon, Button.ripple ]
                [ Icon.i "phone" ]
            ]
        ]
