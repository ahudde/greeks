test_that("implied volatility is correct", {

  # The implied volatility is computed with different parameters, one of them
  # being `option_price`. The resulting `implied_volatility` is the used to
  # compute the `option_price_test`. Both values should be very close.

  number_of_runs <- 4

  # the parameters are chosen at random
  set.seed(42)
  initial_price <- runif(number_of_runs, 90, 110)
  exercise_price <- runif(number_of_runs, 90, 110)
  r <- runif(number_of_runs, -0.01, 0.1)
  time_to_maturity <- runif(number_of_runs, 0.2, 6)
  dividend_yield <- runif(number_of_runs, 0, 0.1)
  model <- "Black_Scholes"
  option_type <- c("European", "European", "Geometric Asian", "Geometric Asian")
  payoff <- rep(c("call", "put"), 2)
  volatility_to_compute_initial_price <- runif(number_of_runs, 0.01, 1)

  option_price <- numeric(number_of_runs)
  option_price_zero_vol <- numeric(number_of_runs)
  option_price_test <- numeric(number_of_runs)

  implied_volatility <- numeric(number_of_runs)

  for(i in 1:number_of_runs) {
    option_price[i] <-
      Greeks(
        initial_price = initial_price[i],
        exercise_price = exercise_price[i],
        r = r[i],
        time_to_maturity = time_to_maturity[i],
        volatility = volatility_to_compute_initial_price[i],
        dividend_yield = dividend_yield[i],
        option_type = option_type[i],
        payoff = payoff[i],
        greek = "fair_value"
      )

    option_price_zero_vol[i] <-
      Greeks(
        initial_price = initial_price[i],
        exercise_price = exercise_price[i],
        r = r[i],
        time_to_maturity = time_to_maturity[i],
        volatility = 1e-12,
        dividend_yield = dividend_yield[i],
        option_type = option_type[i],
        payoff = payoff[i],
        greek = "fair_value"
      )

    # We check if the implied volatility for the chosen parameters is
    # well-defined
    if (option_price[i] <= option_price_zero_vol[i] + 1e-12) {
      option_price[i] <- 0
      next
    }

    implied_volatility[i] <-
      Implied_Volatility(
        option_price = option_price[i],
        initial_price = initial_price[i],
        exercise_price = exercise_price[i],
        r = r[i],
        time_to_maturity = time_to_maturity[i],
        dividend_yield = dividend_yield[i],
        model = model,
        option_type = option_type[i],
        payoff = payoff[i]
      )

    option_price_test[i] <-
      Greeks(
        initial_price = initial_price[i],
        exercise_price = exercise_price[i],
        r = r[i],
        time_to_maturity = time_to_maturity[i],
        volatility = implied_volatility[i],
        dividend_yield = dividend_yield[i],
        model = model,
        option_type = option_type[i],
        payoff = payoff[i],
        greek = "fair_value"
      )

  }

  expect(max(abs(option_price_test - option_price)) < 1e-06,
  "Implied_Volatility error is too high")

  # Checks if error is caught when the option price is infeasible low
  expect_error(Implied_Volatility(option_price = 10, exercise_price = 90))

})

test_that("European implied volatility respects iteration controls", {
  option_price <- BS_European_Greeks(
    initial_price = 100,
    exercise_price = 100,
    r = 0.02,
    time_to_maturity = 1,
    volatility = 0.8,
    dividend_yield = 0.01,
    payoff = "call",
    greek = "fair_value"
  )

  expect_error(
    BS_Implied_Volatility(
      option_price = option_price,
      initial_price = 100,
      exercise_price = 100,
      r = 0.02,
      time_to_maturity = 1,
      dividend_yield = 0.01,
      payoff = "call",
      start_volatility = 0.1,
      precision = 1e-12,
      max_iter = 1
    ),
    "Maximum number of iterations reached (max_iter = 1)",
    fixed = TRUE
  )

  expect_error(
    Implied_Volatility(
      option_price = option_price,
      initial_price = 100,
      exercise_price = 100,
      r = 0.02,
      time_to_maturity = 1,
      dividend_yield = 0.01,
      option_type = "European",
      payoff = "call",
      start_volatility = 0.1,
      precision = 1e-12,
      max_iter = 1
    ),
    "Maximum number of iterations reached (max_iter = 1)",
    fixed = TRUE
  )

  expect_error(
    BS_Implied_Volatility(option_price, max_iter = 0),
    "max_iter must be a positive integer",
    fixed = TRUE
  )
  expect_error(
    Implied_Volatility(option_price, precision = -1),
    "precision must be a positive finite number",
    fixed = TRUE
  )
})

test_that("infeasible high prices terminate at max_iter", {
  expect_error(
    BS_Implied_Volatility(
      option_price = 101,
      initial_price = 100,
      exercise_price = 100,
      r = 0,
      time_to_maturity = 1,
      dividend_yield = 0,
      payoff = "call",
      max_iter = 2
    ),
    "Maximum number of iterations reached (max_iter = 2)",
    fixed = TRUE
  )
})
