options(scipen = 999)

initial_price <- runif(1, min = 0, max = 200)
exercise_price <- initial_price * runif(1, min = 0.7, max = 1/0.7)
r <- runif(1, min = -0.2, max = 1)
time_to_maturity <- runif(1, min = 0, max =25)
volatility <- runif(1, min = 0, max = 1)
dividend_yield <- runif(1, min = -0.1, max = 0.3)
payoff = sample(c("put", "call", "digital_put", "digital_call"), 1)
paths <- round(runif(1, min = 0, max = 100000))
steps <- round(runif(1, min = 1, max = 50))

print(list(
  "initial_price" = initial_price,
  "exercise_price" = exercise_price,
  "r" = r,
  "time_to_maturity" = time_to_maturity,
  "volatility" = volatility,
  "dividend_yield" = dividend_yield,
  "payoff" = payoff,
  "paths" = paths,
  "steps" = steps))


diff <- Malliavin_Asian_Greeks(
  initial_price = initial_price,
  exercise_price = exercise_price,
  r = r,
  time_to_maturity = time_to_maturity,
  volatility = volatility,
  dividend_yield = dividend_yield,
  payoff = payoff) -
  Malliavin_Asian_Greeks_2(initial_price,
                           exercise_price,
                           r,
                           time_to_maturity,
                           volatility,
                           dividend_yield,
                           payoff)

print(list(
  "diff" = diff,
  "initial_price" = initial_price,
  "exercise_price" = exercise_price,
  "r" = r,
  "time_to_maturity" = time_to_maturity,
  "volatility" = volatility,
  "dividend_yield" = dividend_yield,
  "payoff" = payoff,
  "paths" = paths,
  "steps" = steps))

