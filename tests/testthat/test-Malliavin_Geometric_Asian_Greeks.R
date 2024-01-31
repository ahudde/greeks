test_that("Malliavin_Geometric_Asian_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 8

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
    payoff <- rep(c("put", "call"), number_of_runs/2)[i]
    greek <- c("fair_value", "fair_value", "delta", "delta", "vega", "vega", "gamma", "gamma")[i]
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
        steps = 24,
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

  # We check, whether computation for vectorized parameters initial_value and
  # exercise price works

  vectorized_initial_price <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = 99:101,
      exercise_price = exercise_price,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = greek,
      paths = 100)

  single_initial_price_2 <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = 100,
      exercise_price = exercise_price,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = greek,
      paths = 100)

  expect(abs(vectorized_initial_price[2] - single_initial_price_2) < 1e-9,
         "Malliavin_Geometric_Asian_Greeks: Vectorized computation wrt to
         initial_value does not work")

  vectorized_exercise_price <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = initial_price,
      exercise_price = 99:101,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = greek,
      paths = 100)

  single_exercise_price_2 <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = initial_price,
      exercise_price = 100,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = greek,
      paths = 100)

  expect(abs(vectorized_exercise_price[2] - single_exercise_price_2) < 1e-9,
         "Malliavin_Geometric_Asian_Greeks: Vectorized computation wrt to
         exercise_price does not work")

  # We check, whether custom payoff functions work

  digital_call <-
    function(x, exercise_price) {ifelse(x >= exercise_price, 1, 0)}

  expect(max(abs(
    Malliavin_Geometric_Asian_Greeks(payoff = digital_call, paths = 100) -
      Malliavin_Geometric_Asian_Greeks(payoff = "digital_call", paths = 100))) < 1e-9,
    "Malliavin_Geometric_Asian_Greeks: Custom payoff functions do not work")

  digital_put <-
    function(x, exercise_price) {ifelse(x <= exercise_price, 1, 0)}

  expect(max(abs(
    Malliavin_Geometric_Asian_Greeks(payoff = digital_put, paths = 100) -
      Malliavin_Geometric_Asian_Greeks(payoff = "digital_put", paths = 100))) < 1e-9,
    "Malliavin_Geometric_Asian_Greeks: Custom payoff functions do not work")

})
