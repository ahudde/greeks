test_that("Malliavin_European_Greeks is correct", {

  number_of_runs <- 6

  Greeks <- c("fair_value", "delta", "vega", "theta", "rho", "gamma")

  error <- matrix(nrow = number_of_runs, ncol = length(Greeks))

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
    payoff <- c("call", "put", "cash_or_nothing_call", "cash_or_nothing_put",
                "asset_or_nothing_call", "asset_or_nothing_put")[i]
    antithetic <- sample(c(TRUE, FALSE), 1)
    greek <- Greeks

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

    error[i, ] <-
      pmin(abs(Malliavin_Value - Exact_Value)/(abs(Malliavin_Value + epsilon)),
           abs(Malliavin_Value - Exact_Value))

  }

  # "asset_or_nothing_call and asset_or_nothing_put payoff-functions have much
  # more variance
  expect(max(error[1:4]) < 0.01 && max(error[5:6]) < 0.1,
         "The results of Malliavin_European_Greeks() are not close enough to
         BS_European_Greeks()")

  expect_error(Malliavin_European_Greeks(model = "whatever_model"),
               "Malliavin_European_Greeks should throw an error here")

  # Check, whether custom payoff function works

  call_function <- function(x) {
    return(pmax(0, x-100))
  }

  diff <-
    sum(abs(Malliavin_European_Greeks(payoff = call_function) -
              Malliavin_European_Greeks(payoff = "call")))

  expect(abs(diff) < 1e-7, "Custom payoff function does not seem to work")

})
