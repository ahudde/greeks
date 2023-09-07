Binomial_American_Greeks_test <-
  function(initial_price = 50,
           exercise_price = 50,
           r = 0.1,
           time_to_maturity = 5/12,
           volatility = 0.4,
           dividend_yield = 0,
           payoff = "put",
           greek = c("fair_value", "delta", "vega", "theta", "rho", "epsilon",
                     "gamma"),
           steps = 5,
           eps = 1/10000,
           control_variate = TRUE) {

    ## the payoff function ##

    # TODO: das ist hässlich
    payoff_name <- payoff

    if (payoff == "call") {
      payoff <- function(x, exercise_price) {
        return(max(0, x - exercise_price))
      }
    } else if (payoff == "put") {
      payoff <- function(x, exercise_price) {
        return(max(0, exercise_price - x))
      }
    }

    result <- numeric(length(greek)) * NA

    names(result) <- greek

    underlying <- matrix(NA, nrow = steps + 1, ncol = steps + 1)
    american_option_value <- matrix(NA, nrow = steps + 1, ncol = steps + 1)
    european_option_value <- matrix(NA, nrow = steps + 1, ncol = steps + 1)

    # dt is the length of the time step
    dt <- time_to_maturity/steps
    # size of one step up or down
    up <- exp(volatility * sqrt(dt))
    down <- exp(-volatility * sqrt(dt))
    # p is the probability of going one step up
    p <- (exp((r-dividend_yield)*dt) - down) / (up - down)

    print(p)

    # the tree is generated
    underlying[1, 1] <- initial_price

    for (j in 2:(steps+1)) {
      underlying[1, j] <- up * underlying[1, j-1]
      for (i in 2:j) {
        underlying[i, j] <- down * underlying[i-1, j-1]
      }
    }

    # initializing with the prices
    for(i in 1:(steps+1)) {
      american_option_value[i, steps + 1] <- payoff(underlying[i, steps + 1], exercise_price)
      european_option_value[i, steps + 1] <- payoff(underlying[i, steps + 1], exercise_price)
    }

    for (j in steps:1) {
      for (i in 1:j) {
        american_option_value[i, j] <-
          max(
            (american_option_value[i, j+1] * p + american_option_value[i+1, j+1] * (1-p)) * exp(-r*dt),
            payoff(underlying[i, j], exercise_price))
        european_option_value[i, j] <-
          (european_option_value[i, j+1] * p + european_option_value[i+1, j+1] * (1-p)) * exp(-r*dt)
      }
    }

    if(control_variate == FALSE) {
      return(american_option_value[1, 1])
    } else {
      return(
      american_option_value[1, 1] - european_option_value[1, 1] +
        BS_European_Greeks(initial_price = initial_price,
                           exercise_price = exercise_price,
                           r = r,
                           time_to_maturity = time_to_maturity,
                           volatility = volatility,
                           dividend_yield = dividend_yield,
                           payoff = payoff_name,
                           greek = "fair_value"))
    }

  }
