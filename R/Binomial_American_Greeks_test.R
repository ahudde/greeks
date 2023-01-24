american_call <- function(x, price, exercise_price) {
  max(x, price - exercise_price)
}

american_put <- function(x, price, exercise_price) {
  max(x, exercise_price - price)
}

Binomial_American_Greeks_test <- function(
    initial_price = 100,
    exercise_price = 100,
    r = 0,
    time_to_maturity = 1,
    volatility = 0.3,
    dividend_yield = 0,
    payoff = "call",
    steps = 1000) {

  dt <- time_to_maturity/steps

  up <- exp(volatility * sqrt(dt))
  down <- exp(-volatility * sqrt(dt))
  p <- (exp((r - dividend_yield)*dt) - down)/(up - down)
  p_ <- exp(-r*dt)*p
  q_ <- exp(-r*dt)*(1 - p)

  # generate the price vector

  price <- numeric(2*steps + 1)

  for (j in 1:(2*steps+1)) {
    price[j] = initial_price * exp(-volatility * (steps - j - 1) * sqrt(dt))
  }

  # generate the value matrix

  value <- numeric(steps + 1)

  if (payoff == "call") {
    payoff_function <- american_call
  } else if (payoff == "put") {
    payoff_function <- american_put
  }

  for (i in 1:(steps + 1)) {
    value[i] <- payoff_function(0.0, price[2*steps - 2*i + 3], exercise_price)
  }

  for (j in steps:2) {
    for (i in 1:j) {
      value[i] <- payoff_function(p_ * value[i] + q_ * value[i + 1],
                                 price[2*steps - 2*i + j + 1 - steps], exercise_price)
    }
  }

  result <- payoff_function(
    p_ * value[1] + q_ * value[2],
    price[steps + 1],
    exercise_price)

  return(result)

}
