{
  initial_price = runif(1, 0, 200)
  exercise_price = runif(1, 0, 200)
  r = runif(1, -0.3, 0.3)
  time_to_maturity = runif(1, 0, 20)
  dividend_yield = runif(1, 0, 0.2)
  payoff = sample(c("call", "put"), 1)
  option_price = runif(1, 0, 100)

  print(option_price)

  BS_European_Greeks(
    initial_price = initial_price,
    exercise_price = exercise_price,
    r = r,
    time_to_maturity = time_to_maturity,
    dividend_yield = dividend_yield,
    payoff = payoff,
    volatility = BS_Implied_Volatility(option_price = option_price),
    greek = "fair_value") |>
    print()

}
