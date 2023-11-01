# TODO: Digital options

test_that("Malliavin_European_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 30

  Greeks <- c("fair_value", "delta", "vega", "theta", "rho", "gamma")

  error <- numeric(number_of_runs)

  set.seed(42)

  epsilon <- 1e-5

  for(i in 1:number_of_runs) {

    # the parameters are chosen at random
    initial_price <- runif(1, 90, 110)
    exercise_price <- runif(1, 90, 110)
    r <- runif(1, -0.01, 0.1)
    time_to_maturity <- runif(1, 0.2, 6)
    volatility <- runif(1, 0.01, 1)
    model <- "Black_Scholes"
    payoff <- sample(
      c("call", "put"), 1)
    greek <- sample(Greeks, 1)

    Malliavin_Value <-
      Malliavin_European_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        payoff = payoff,
        greek = greek,
        paths = 1000000
      )

    Exact_Value <-
      BS_European_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = 0,
        payoff = payoff,
        greek = greek
      )

    error[i] <-
      min(abs(Malliavin_Value - Exact_Value)/(abs(Malliavin_Value + epsilon)),
          abs(Malliavin_Value - Exact_Value))

  }

  expect(max(error) < 0.01)

})
