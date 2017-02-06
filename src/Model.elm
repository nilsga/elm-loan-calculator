module Model exposing (..)

import LoanCalulations.LoanDetails as LoanDetails


type alias Model =
    { loanDetails : LoanDetails
    , calculatedTermAmount : Float
    }
