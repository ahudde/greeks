test_that("Malliavin_European_Greeks is correct", {

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
      c("call", "put", "cash_or_nothing_call", "cash_or_nothing_put"), 1)
    antithetic <- sample(c(TRUE, FALSE), 1)
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
        paths = 2000000,
        antithetic = antithetic
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
  expect_error(Malliavin_European_Greeks(model = "whatever_model"))

  # Check, whether custom payoff function works

  call_function <- function(x) {
    return(pmax(0, x-100))
  }

  diff <-
    sum(abs(Malliavin_European_Greeks(payoff = call_function) -
              Malliavin_European_Greeks(payoff = "call")))

  expect(abs(diff) < 1e-7)

})
