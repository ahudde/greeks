test_that("BS_Geometric_Asian_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 100

  definition_of_geeks <-
    tibble(
      greek = c("delta", "gamma", "vega", "vomma", "theta", "rho"),
      start = c("fair_value", "delta", "fair_value", "vega", "fair_value",
                "fair_value"),
      param = c("initial_price", "initial_price", "volatility", "volatility",
                "time_to_maturity", "r")
    )

  error <- numeric(number_of_runs)

  set.seed(42)

  ## TODO: epsilon is a bad variable name, since is can be confused with the
  ## Greek epsilon, also change that in test-BS_European_Greeks
  epsilon <- 1e-5

  for(i in 1:number_of_runs) {

    # the parameters are chosen at random
    initial_price <- runif(1, 90, 110)
    exercise_price <- runif(1, 90, 110)
    r <- runif(1, -0.01, 0.1)
    time_to_maturity <- runif(1, 0.2, 6)
    dividend_yield <- runif(1, 0, 0.1)
    volatility <- runif(1, 0.001, 1)
    model <- "Black_Scholes"
    payoff <- sample(c("call", "put"), 1)
    greek <- sample(definition_of_geeks$greek, 1)
    param <-
      definition_of_geeks[definition_of_geeks$greek == greek, "param"] %>%
      as.character()
    start <-
      definition_of_geeks[definition_of_geeks$greek == greek, "start"] %>%
      as.character()

    Vals <-
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

    if (param == "time_to_maturity") {
      Vals = -Vals
    }

    F <- function(epsilon) {
      assign(param, get(param) + epsilon)
      BS_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = start
      )

    }

    Vals_fd <- (F(epsilon) - F(-epsilon)) / (2 * epsilon)
    error[i] <-
      min(abs(Vals - Vals_fd)/(abs(Vals + epsilon)),
          abs(Vals - Vals_fd))
  }

  expect(max(error) < sqrt(epsilon))

})
