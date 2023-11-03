test_that("Greeks is correct", {

  expect_error(
    Greeks(
      initial_price = 101,
      exercise_price = 99,
      r = 0.01,
      time_to_maturity = 13/12,
      volatility = 0.3,
      dividend_yield = 0,
      model = "Black_Scholes",
      payoff = "call",
      option_type = "digital"
    )
  )

  expect_error(
    Greeks(
      initial_price = 101,
      exercise_price = 99,
      r = 0.01,
      time_to_maturity = 13/12,
      volatility = 0.3,
      dividend_yield = 0,
      model = "Black_Scholes",
      payoff = "call",
      option_type = "lookback"
    )
  )

})

