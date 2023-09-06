test_that("Binomial_American_Greeks is correct", {

  # We check the Greeks by also computing the derivative with finite difference
  # and comparing the results

  number_of_runs <- 50

  definition_of_greeks <-
    data.frame(greek = "delta", start = "fair_value", param = "initial_price") %>%
    add_row(greek = "epsilon", start = "fair_value", param = "dividend_yield") %>%
    # add_row(greek = "gamma", start = "delta", param = "initial_price") %>%
    add_row(greek = "rho", start = "fair_value", param = "r") %>%
    add_row(greek = "theta", start = "fair_value", param = "time_to_maturity") %>%
    add_row(greek = "vega", start = "fair_value", param = "volatility")

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
    payoff <- sample(c("call", "put"), 1)
    greek <- sample(definition_of_greeks$greek, 1)
    param <-
      definition_of_greeks[definition_of_greeks$greek == greek, "param"] %>%
      as.character()
    start <-
      definition_of_greeks[definition_of_greeks$greek == greek, "start"] %>%
      as.character()

    Vals <-
      Greeks(
        option_type = "American",
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
      Greeks(
        option_type = "American",
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
        Greeks(
          option_type = "American",
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

  expect(max(error) < 0.1)

})

test_that("Binomial_American_Greeks has correst fair_value", {

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


          })
