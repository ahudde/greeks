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
#' @param option_type in c("European", "American", "Asian", "Geometric Asian",
#' "Digital", "Binomial) - the type of option to be considered
#' @param payoff - in c("call", "put", "cash_or_nothing_call",
#' "cash_or_nothing_put", "asset_or_nothing_call", "asset_or_nothing_put")
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna", "charm", "vomma",
#' "veta", "vera", "speed", "zomma", "color", "ultima")
#' @param ... - ...	Other arguments passed on to methods
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
           greek = c("fair_value", "delta", "vega", "theta", "rho", "gamma"),
           ...){

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
                                      dividend_yield = dividend_yield,
                                      ...))
    }

    else if (tolower(option_type) == "geometric asian" && tolower(model) == "black_scholes") {
      return(BS_Geometric_Asian_Greeks(payoff = payoff,
                                       greek = greek,
                                       initial_price = initial_price,
                                       exercise_price = exercise_price,
                                       r = r,
                                       time_to_maturity = time_to_maturity,
                                       volatility = volatility,
                                       dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "asian" && tolower(model) == "black_scholes" &&
             unique(greek)[1] == "fair_value" && unique(greek)[2] == "delta" &&
             unique(greek)[3] == "rho" && length(greek) == 3) {
      return(BS_Malliavin_Asian_Greeks(payoff = payoff,
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
      if (tolower(payoff) %in% c("call", "put")) {
        stop("Please choose one of the following:
                'cash_or_nothing_call',
                'cash_or_nothing_put',
                'asset_or_nothing_call',
                'asset_or_nothing_put'.")
        return()
      }
      return(BS_European_Greeks(payoff = payoff,
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
