test_that("BS_Malliavin_Asian_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 2

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
    time_to_maturity <- runif(1, 0.2, 6)
    dividend_yield <- runif(1, 0, 0.1)
    volatility <- runif(1, 0.01, 1)
    model <- "Black_Scholes"
    payoff <- rep(c("call", "put"), 3)[i]
    greek <- rep(c("delta"), 2)[i]
    param <-
      definition_of_greeks[definition_of_greeks$greek == greek, "param"] %>%
      as.character()
    start <-
      definition_of_greeks[definition_of_greeks$greek == greek, "start"] %>%
      as.character()

    Vals <-
      BS_Malliavin_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = greek,
        paths = 10000,
        steps = 12
      )

    ## theta is minus the derivative of fair_value w.r.t. time_to_maturity
    if (greek == "theta") {
      Vals = -Vals
    }

    F <- function(epsilon) {
      assign(param, get(param) + epsilon)
      BS_Malliavin_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = start,
        paths = 10000,
        steps = 12
      )
    }

    epsilon <- get(param) * 0.1

    Vals_fd <- (F(epsilon) - F(-epsilon)) / (2 * epsilon)


    error[i] <-
      min(abs(Vals - Vals_fd)/(abs(Vals + epsilon)),
          abs(Vals - Vals_fd))

  }

  expect(max(error) < 0.1)

})
