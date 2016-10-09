module LoanCalculations exposing(..)

type alias LoanDetails = {amount: Float, interestRate: Float, terms: Float, duration: Float, initialFee: Float, termFee: Float}
type alias Term = {amount: Float, principal: Float, interest: Float, balance: Float}

termInterest: LoanDetails -> Float
termInterest {interestRate, terms} =
  interestRate / terms

termAmount: LoanDetails -> Float
termAmount loan =
  let
    interestPerTerm = termInterest loan
    {amount, duration, terms, termFee} = loan
  in
    amount * interestPerTerm / (1 - (1 + interestPerTerm) ^ (-(duration) * terms)) + termFee

term: LoanDetails -> Float -> Term
term loan term =
   let
      previousTermBalance = termBalance loan (term - 1)
      balance = termBalance loan term
      principal = previousTermBalance - balance
      amount = termAmount loan
      interest = amount - principal
   in
      {amount = amount, principal = principal, interest = interest, balance = balance}

termBalance: LoanDetails -> Float -> Float
termBalance loan term =
   let
      interestPerTerm = termInterest loan
      {amount} = loan
   in
      amount * (1 + interestPerTerm) ^ term - (termAmount loan / interestPerTerm) * ((1 + interestPerTerm) ^ term - 1)


