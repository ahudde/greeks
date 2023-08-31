test_that("BS_European_Greeks is correct", {

  # TODO: more Greeks

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 100

  dict_greeks_params <- list(
    initial_price = "delta",
    volatility = "vega",
    time_to_maturity = "theta",
    r = "rho",
    dividend_yield = "epsilon"
  )

  error <- numeric(number_of_runs)

  set.seed(42)

  for(i in 1:number_of_runs) {

    # the parameters are chosen at random
    initial_price <- runif(1, 90, 110)
    exercise_price <- runif(1, 90, 110)
    r <- runif(1, -0.01, 0.1)
    time_to_maturity <- runif(1, 0.2, 6)
    dividend_yield <- runif(1, 0, 0.1)
    volatility <- runif(1, 0, 1)
    model <- "Black_Scholes"
    payoff <- sample(c("call", "put"), 1)
    param <- sample(names(dict_greeks_params), 1)

    epsilon <- 1e-5
    Vals <-
      BS_European_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = dict_greeks_params[[param]]
      )

    if (param == "time_to_maturity") {
      Vals = -Vals
    }

    F <- function(epsilon) {
      assign(param, get(param) + epsilon)
      BS_European_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = "fair_value"
      )

    }

    Vals_fd <- (F(epsilon) - F(-epsilon)) / (2 * epsilon)
    error[i] <- abs(Vals - Vals_fd)/(abs(Vals + epsilon))

  }

  expect(max(error) < sqrt(epsilon))

})
