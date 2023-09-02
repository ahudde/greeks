test_that("BS_European_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 200

  definition_of_geeks <-
    data.frame(greek = "charm", start = "theta", param = "initial_price") %>%
    add_row(greek = "delta", start = "fair_value", param = "initial_price") %>%
    add_row(greek = "epsilon", start = "fair_value", param = "dividend_yield") %>%
    add_row(greek = "gamma", start = "delta", param = "initial_price") %>%
    add_row(greek = "rho", start = "fair_value", param = "r") %>%
    add_row(greek = "theta", start = "fair_value", param = "time_to_maturity") %>%
    add_row(greek = "vanna", start = "delta", param = "volatility") %>%
    add_row(greek = "vega", start = "fair_value", param = "volatility") %>%
    #add_row(greek = "vera", start = "rho", param = "volatility") %>%
    #add_row(greek = "veta", start = "vega", param = "time_to_maturity") %>%
    #add_row(greek = "vomma", start = "vega", param = "volatility") %>%
    add_row(greek = "speed", start = "gamma", param = "initial_price") %>%
    add_row(greek = "zomma", start = "vanna", param = "initial_price") #%>%
    #add_row(greek = "color", start = "gamma", param = "time_to_maturity") %>%
    #add_row(greek = "ultima", start = "vomma", param = "volatility")

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
    volatility <- runif(1, 0.001, 1)
    model <- "Black_Scholes"
    payoff <- sample(c("call", "put",
                       "cash_or_nothing_call", "cash_or_nothing_put",
                       "asset_or_nothing_call", "asset_or_nothing_put"), 1)
    greek <- sample(definition_of_geeks$greek, 1)
    param <-
      definition_of_geeks[definition_of_geeks$greek == greek, "param"] %>%
      as.character()
    start <-
      definition_of_geeks[definition_of_geeks$greek == greek, "start"] %>%
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
        greek = start
      )

    }

    Vals_fd <- (F(epsilon) - F(-epsilon)) / (2 * epsilon)
    error[i] <-
      error[i] <-
      min(abs(Vals - Vals_fd)/(abs(Vals + epsilon)),
          abs(Vals - Vals_fd))
    # TODO: diesen Teil löschen
    if(error[i] > sqrt(epsilon)){
      print(greek)
      print(param)
      print(error[i])
    }
  }

  expect(max(error) < sqrt(epsilon))

})
