Implied_Volatility <-
  function(option_prize,
           initial_price = 100,
           exercise_price = 100,
           r = 0,
           time_to_maturity = 1,
           dividend_yield = 0,
           payoff = "call") {
    f <- function(volatility) {
      BS_European_Greeks(initial_price = initial_price,
                         )
    }
  }
