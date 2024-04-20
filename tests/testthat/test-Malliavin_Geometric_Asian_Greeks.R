test_that("Malliavin_Geometric_Asian_Greeks is correct", {

  # We check the Greeks by comparing with the exact results
  number_of_runs <- 8

  Greeks <- c("fair_value", "delta", "theta", "rho", "gamma")

  error <- matrix(nrow = number_of_runs, ncol = length(Greeks))

  set.seed(42)

  for(i in 1:number_of_runs) {

    # the parameters are chosen at random
    initial_price <- runif(1, 90, 110)
    exercise_price <- runif(1, 90, 110)
    r <- runif(1, -0.01, 0.1)
    time_to_maturity <- runif(1, 0.2, 1.5)
    dividend_yield <- runif(1, 0, 0.1)
    volatility <- runif(1, 0.01, 1)
    model <- "Black_Scholes"
    payoff <- rep(c("put", "call"), number_of_runs/2)[i]
    antithetic <- sample(c("call", "put"), 1)

    Value_MC <-
      Malliavin_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = Greeks,
        paths = 1000000,
        steps = 48,
        antithetic = antithetic
      )

    Value_exact <-
      BS_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = Greeks
      )

    for(j in 1:length(Greeks)) {
      error[i, j] <-
      min(abs(Value_MC[j] - Value_exact[j])/(abs(Value_MC[j]) + 1e-5),
          abs(Value_MC[j] - Value_exact[j]))
    }

  }

  expect(
    max(error) < 0.1,
    failure_message = "The results of Malliavin_Geometric_Asian_Greeks.R cannot
    be confirmend by BS_Geometric_Asian_Greeks.R")

  # We check, whether computation for vectorized parameters initial_value and
  # exercise price works

  vectorized_initial_price <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = 99:101,
      exercise_price = exercise_price,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = Greeks,
      paths = 100)

  single_initial_price <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = 100,
      exercise_price = exercise_price,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = Greeks,
      paths = 100)

  expect(max(abs(vectorized_initial_price[2, ] - single_initial_price)) < 1e-9,
         failure_message = "Malliavin_Geometric_Asian_Greeks: Vectorized
         computation wrt to initial_value does not work")

  vectorized_exercise_price <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = initial_price,
      exercise_price = 99:101,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = Greeks,
      paths = 100)

  single_exercise_price <-
    Malliavin_Geometric_Asian_Greeks(
      initial_price = initial_price,
      exercise_price = 100,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = Greeks,
      paths = 100)

  expect(
    max(abs(vectorized_exercise_price[2, ] - single_exercise_price)) < 1e-9,
    failure_message =  "Malliavin_Geometric_Asian_Greeks: Vectorized computation
    wrt to exercise_price does not work")

  # We check, whether custom payoff functions work

  digital_call <-
    function(x, exercise_price) {ifelse(x >= exercise_price, 1, 0)}

  expect(
    max(abs(
      Malliavin_Geometric_Asian_Greeks(payoff = digital_call, paths = 100) -
        Malliavin_Geometric_Asian_Greeks(payoff = "digital_call", paths = 100))) < 1e-9,
    failure_message =  "Malliavin_Geometric_Asian_Greeks: Custom payoff
    functions do not work")

  digital_put <-
    function(x, exercise_price) {ifelse(x <= exercise_price, 1, 0)}

  expect(max(abs(
    Malliavin_Geometric_Asian_Greeks(payoff = digital_put, paths = 100) -
      Malliavin_Geometric_Asian_Greeks(payoff = "digital_put", paths = 100))) < 1e-9,
    failure_message =  "Malliavin_Geometric_Asian_Greeks: Custom payoff
    functions do not work")

})
