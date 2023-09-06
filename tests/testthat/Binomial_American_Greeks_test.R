Binomial_American_Greeks_test <-
  function(initial_price = 50,
           exercise_price = 50,
           r = 0.1,
           time_to_maturity = 5/12,
           volatility = 0.4,
           dividend_yield = 0,
           payoff = "call",
           greek = c("fair_value", "delta", "vega", "theta", "rho", "epsilon",
                     "gamma"),
           steps = 1000,
           eps = 1/10000,
           control_variate = TRUE) {

    result <- numeric(length(greek)) * NA

    names(result) <- greek

    underlying <- matrix(NA, nrow = steps + 1, ncol = steps + 1)
    option_value <- matrix(NA, nrow = steps + 1, ncol = steps + 1)

    # dt is the length of the time step
    dt <- time_to_maturity/steps
    # size of one step up or down
    up <- exp(volatility * sqrt(dt))
    down <- exp(-volatility * sqrt(dt))
    # p is the probability of going one step up
    p <- (exp((r-dividend_yield)*dt) - down) / (up - down)
    # TODO
    p_ <- exp(-r*dt)*p
    q_ <- exp(-r*dt)*(1-p)

    # the tree is generated
    underlying[1, 1] <- initial_price

    for (j in 2:(steps+1)) {
      underlying[1, j] <- up * underlying[1, j-1]
      for (i in 2:j) {
        underlying[i, j] <- down * underlying[i-1, j-1]
      }
    }

    print(underlying)

    # put, noch ändern TODO
    for(i in 1:(steps+1)) {
      option_value[i, steps + 1] <- max(0, - underlying[i, steps + 1] + exercise_price)
    }



    for (j in steps:1) {
      for (i in 1:j) {
        option_value[i, j] <-
          max(
            (option_value[i, j+1] * p + option_value[i+1, j+1] * (1-p)) * exp(-r*dt),
            exercise_price - underlying[i, j])
      }
    }

    print(option_value)

  }

Binomial_American_Greeks_test(steps = 5)
