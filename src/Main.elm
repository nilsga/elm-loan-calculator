import Html exposing (..)
import Html.Events exposing(onInput)
import Html.Attributes exposing(placeholder)
import LoanCalculations exposing (..)
import String

main: Program Never Model Msg
main =
  beginnerProgram {
    model = model,
    view = view,
    update = update
  }

type alias Model = LoanDetails
type Msg = LoanAmount String | LoanInterestRate String | LoanDuration String

model: Model
model = {amount = 2000000, interestRate = 0.025, terms = 12, duration = 20, termFee = 0, initialFee = 0}

update: Msg -> Model -> Model
update msg oldModel =
  case msg of
    LoanAmount newAmount ->
      { oldModel | amount = Result.withDefault 2000000 (String.toInt newAmount) }
    LoanInterestRate newInterestRate ->
      { oldModel | interestRate = Result.withDefault 0.025 (String.toFloat newInterestRate) }
    LoanDuration newDuration ->
      { oldModel | duration = Result.withDefault 20 (String.toInt newDuration) }


view: Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Lånebeløp", onInput LoanAmount] []
    , input [ placeholder "Rente", onInput LoanInterestRate] []
    , input [ placeholder "Nedbetalingstid", onInput LoanDuration] []
    , div [] [ text (toString (termAmount model)) ]
    ]

