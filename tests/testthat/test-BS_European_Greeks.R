test_that("BS_European_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 300

  definition_of_greeks <-
    data.frame(greek = "charm", start = "theta", param = "initial_price") %>%
    add_row(greek = "delta", start = "fair_value", param = "initial_price") %>%
    add_row(greek = "epsilon", start = "fair_value", param = "dividend_yield") %>%
    add_row(greek = "gamma", start = "delta", param = "initial_price") %>%
    add_row(greek = "rho", start = "fair_value", param = "r") %>%
    add_row(greek = "theta", start = "fair_value", param = "time_to_maturity") %>%
    add_row(greek = "vanna", start = "delta", param = "volatility") %>%
    add_row(greek = "vega", start = "fair_value", param = "volatility") %>%
    add_row(greek = "vera", start = "rho", param = "volatility") %>%
    add_row(greek = "veta", start = "vega", param = "time_to_maturity") %>%
    add_row(greek = "vomma", start = "vega", param = "volatility") %>%
    add_row(greek = "speed", start = "gamma", param = "initial_price") %>%
    add_row(greek = "zomma", start = "vanna", param = "initial_price") %>%
    add_row(greek = "color", start = "gamma", param = "time_to_maturity") %>%
    add_row(greek = "ultima", start = "vomma", param = "volatility") %>%
    add_row(greek = "lambda", start = "fair_value", param = "initial_price")

  error <- numeric(number_of_runs)

  set.seed(42)

  epsilon <- 1e-5

  for(i in 1:number_of_runs) {

    # the parameters are chosen at random
    initial_price <- runif(1, 90, 110)
    exercise_price <- runif(1, 90, 110)
    r <- runif(1, -0.01, 0.1)
    time_to_maturity <- runif(1, 0.2, 6)
    dividend_yield <- runif(1, 0, 0.1)
    volatility <- runif(1, 0.01, 1)
    model <- "Black_Scholes"
    payoff <- sample(c("call", "put",
                       "cash_or_nothing_call", "cash_or_nothing_put",
                       "asset_or_nothing_call", "asset_or_nothing_put"), 1)
    greek <- sample(definition_of_greeks$greek, 1)
    param <-
      definition_of_greeks[definition_of_greeks$greek == greek, "param"] %>%
      as.character()
    start <-
      definition_of_greeks[definition_of_greeks$greek == greek, "start"] %>%
      as.character()

    Vals <-
      BS_European_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = greek
      )

    ## theta is minus the derivative of fair_value w.r.t. time_to_maturity
    if (greek == "theta") {
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
        greek = start
      )
    }

    Vals_fd <- (F(epsilon) - F(-epsilon)) / (2 * epsilon)

    ## lambda is delta * initial_price / fair_value
    if(greek == "lambda") {
      Vals_fd <- Vals_fd *  initial_price /
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

    error[i] <-
      min(abs(Vals - Vals_fd)/(abs(Vals + epsilon)),
          abs(Vals - Vals_fd))

  }

  expect(max(error) < sqrt(epsilon), "Error in BS_European_Greeks is too high")

})

test_that("BS_European_Greeks rejects invalid positive model parameters", {
  invalid_positive_params <- list(
    initial_price = 0,
    exercise_price = 0,
    time_to_maturity = 0,
    volatility = 0
  )

  for (param in names(invalid_positive_params)) {
    args <- list(greek = "fair_value")
    args[[param]] <- invalid_positive_params[[param]]

    expect_error(
      do.call(BS_European_Greeks, args),
      paste0(param, " must be a positive finite number"),
      fixed = TRUE
    )
  }

  expect_error(
    BS_European_Greeks(initial_price = c(100, 101), greek = "fair_value"),
    "initial_price must be a positive finite number",
    fixed = TRUE
  )

  expect_error(
    BS_European_Greeks(volatility = Inf, greek = "fair_value"),
    "volatility must be a positive finite number",
    fixed = TRUE
  )
})

test_that("BS_European_Greeks rejects invalid finite model parameters", {
  expect_error(
    BS_European_Greeks(r = c(0.01, 0.02), greek = "fair_value"),
    "r must be a finite number",
    fixed = TRUE
  )

  expect_error(
    BS_European_Greeks(r = Inf, greek = "fair_value"),
    "r must be a finite number",
    fixed = TRUE
  )

  expect_error(
    BS_European_Greeks(dividend_yield = NA_real_, greek = "fair_value"),
    "dividend_yield must be a finite number",
    fixed = TRUE
  )
})

test_that("BS_European_Greeks rejects duplicate greek inputs", {
  expect_error(
    BS_European_Greeks(greek = c("fair_value", "fair_value")),
    "greek must not contain duplicate values",
    fixed = TRUE
  )
})
