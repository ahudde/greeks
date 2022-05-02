#' Computes the Greeks of various options
#'
#' @export
#'
#' @param initial_price - initial price of the underlying asset
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param dividend_yield - dividend yield
#' @param volatility - volatility of the underlying asset
#' @param model - the model to be chosen
#' @param option_type in c("European", "American", "Asian", "Digital") - the
#' type of option to be considered
#' @param payoff - in c("call", "put")
#' @param greek - greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna")
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'

Greeks <-
  function(initial_price,
           exercise_price,
           r,
           time_to_maturity,
           volatility,
           dividend_yield = 0,
           model = "Black_Scholes",
           option_type = "European",
           payoff = "call",
           greek = c("fair_value", "delta", "vega", "theta", "rho", "gamma")){

    if (tolower(option_type) == "european" && tolower(model) == "black_scholes") {
      return(BS_European_Greeks(payoff = payoff,
                                greek = greek,
                                initial_price = initial_price,
                                exercise_price = exercise_price,
                                r = r,
                                time_to_maturity = time_to_maturity,
                                volatility = volatility,
                                dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "american" && tolower(model) == "black_scholes") {
      return(Binomial_American_Greeks(payoff = payoff,
                                      greek = greek,
                                      initial_price = initial_price,
                                      exercise_price = exercise_price,
                                      r = r,
                                      time_to_maturity = time_to_maturity,
                                      volatility = volatility,
                                      dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "asian" && tolower(model) == "black_scholes") {
      return(Malliavin_Asian_Greeks(payoff = payoff,
                                    greek = greek,
                                    initial_price = initial_price,
                                    exercise_price = exercise_price,
                                    r = r,
                                    time_to_maturity = time_to_maturity,
                                    volatility = volatility,
                                    dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "asian" && tolower(model) == "black_scholes") {
      return(Malliavin_Asian_Greeks(payoff = payoff,
                                    greek = greek,
                                    initial_price = initial_price,
                                    exercise_price = exercise_price,
                                    r = r,
                                    time_to_maturity = time_to_maturity,
                                    volatility = volatility,
                                    dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "digital" && tolower(model) == "black_scholes") {
      if (tolower(payoff) == "call") {
        payoff <- "digital_call"
      } else if (tolower(payoff) == "put") {
        payoff <- "digital_put"
      } else {
        stop("Wrong input or not yet implemented.")
      }
      return(Malliavin_European_Greeks(payoff = payoff,
                                       greek = greek,
                                       initial_price = initial_price,
                                       exercise_price = exercise_price,
                                       r = r,
                                       time_to_maturity = time_to_maturity,
                                       volatility = volatility,
                                       dividend_yield = dividend_yield))
    }

    else {
      stop("Wrong input or not yet implemented.")
    }

}
