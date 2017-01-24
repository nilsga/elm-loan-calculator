module LoanCalculations exposing(..)

type alias LoanDetails = {amount: Int, interestRate: Float, terms: Int, duration: Int, initialFee: Int, termFee: Int}
type alias Term = {amount: Int, principal: Int, interest: Int, balance: Int}
type alias Loan = List Term

termInterest: LoanDetails -> Float
termInterest {interestRate, terms} =
  interestRate / (toFloat terms)

termAmount: LoanDetails -> Float
termAmount loan =
  let
    interestPerTerm = termInterest loan
    {amount, duration, terms, termFee} = loan
  in
    (toFloat amount) * interestPerTerm / (1 - (1 + interestPerTerm) ^ (toFloat(-(duration) * terms))) + (toFloat termFee)

term: LoanDetails -> Float -> Term
term loan term =
   let
      previousTermBalance = termBalance loan (term - 1)
      balance = termBalance loan term
      principal = previousTermBalance - balance
      amount = termAmount loan
      interest = amount - principal
   in
      {amount = round amount, principal = round principal, interest = round interest, balance = round balance}

terms: LoanDetails -> Loan
terms loan =
  let
    {terms, duration} = loan
  in
    List.map (\n -> term loan (toFloat n)) <| List.range 0 (terms * duration)

termBalance: LoanDetails -> Float -> Float
termBalance loan term =
   let
      interestPerTerm = termInterest loan
      {amount} = loan
   in
      (toFloat amount) * (1 + interestPerTerm) ^ term - (termAmount loan / interestPerTerm) * ((1 + interestPerTerm) ^ term - 1)


