test_that("Malliavin_Geometric_Asian_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 6

  definition_of_greeks <-
    data.frame(greek = "delta", start = "fair_value", param = "initial_price") %>%
    add_row(greek = "rho", start = "fair_value", param = "r") %>%
    add_row(greek = "vega", start = "fair_value", param = "volatility")

  error <- numeric(number_of_runs)

  set.seed(42)

  for(i in 1:number_of_runs) {

    # the parameters are chosen at random
    initial_price <- runif(1, 90, 110)
    exercise_price <- runif(1, 90, 110)
    r <- runif(1, -0.01, 0.1)
    time_to_maturity <- runif(1, 0.2, 1.5)
    dividend_yield <- runif(1, 0, 0.1)
    volatility <- runif(1, 0.01, 1)
    model <- "Black_Scholes"
    greeks <- c("fair_value", "delta", "rho", "vega", "theta", "gamma")
    payoff <- rep(c("put", "call"), 3)[i]
    greek <- c("fair_value", "fair_value", "delta", "delta", "vega", "vega")[i]
    antithetic <- c(TRUE, FALSE)[i]
    param <-
      definition_of_greeks[definition_of_greeks$greek == greek, "param"] %>%
      as.character()
    start <-
      definition_of_greeks[definition_of_greeks$greek == greek, "start"] %>%
      as.character()

    Value_MC <-
      Malliavin_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = greek,
        paths = 100000,
        steps = 12,
        antithetic = antithetic
      )

    Value_exact <-
      BS_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = greek
      )


    error[i] <-
      min(abs(Value_MC - Value_exact)/(abs(Value_MC) + 1e-5), #TODO: Klammer hier überall richtig machen
          abs(Value_MC - Value_exact))

  }

  expect(max(error) < 0.1)

})
