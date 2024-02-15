#' Computes the Greeks of various options in the Black Scholes model or both in
#' the Black Scholes model or a Jump Diffusion model in the case of Asian
#' Options, or in the Binomial options pricing model
#'
#' @export
#'
#' @param initial_price - initial price of the underlying asset
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param dividend_yield - dividend yield
#' @param volatility - volatility of the underlying asset
#' @param model - the model to be chosen in ("black_scholes", "jump_diffusion")
#' @param option_type in c("European", "American", "Asian", "Geometric Asian",
#' "Digital", "Binomial) - the type of option to be considered
#' @param payoff - in c("call", "put", "cash_or_nothing_call",
#' "cash_or_nothing_put", "asset_or_nothing_call", "asset_or_nothing_put")
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna", "charm", "vomma",
#' "veta", "vera", "speed", "zomma", "color", "ultima")
#' @param ... - ...	Other arguments passed on to methods
#' @param antithetic - if TRUE, antithetic random numbers will be chosen to
#' decrease variance
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#'
#' @examples
#' Greeks(initial_price = 100, exercise_price = 120, r = 0.01,
#' time_to_maturity = 5, volatility = 0.30, payoff = "call")
#'
#' Greeks(initial_price = 100, exercise_price = 100, r = -0.005,
#' time_to_maturity = 1, volatility = 0.30, payoff = "put",
#' option_type = "American")

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
           antithetic = TRUE,
           ...){

    if (tolower(option_type) %in% c("digital", "european") &&
        tolower(model) == "black_scholes") {
      if (tolower(option_type) == "digital" &&
          tolower(payoff) %in% c("call", "put")) {
        stop("Please choose one of the following:
                'cash_or_nothing_call',
                'cash_or_nothing_put',
                'asset_or_nothing_call',
                'asset_or_nothing_put'.")
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
             all(greek %in% c("fair_value", "delta", "rho", "vega"))) {
      return(BS_Malliavin_Asian_Greeks(payoff = payoff,
                                       greek = greek,
                                       initial_price = initial_price,
                                       exercise_price = exercise_price,
                                       r = r,
                                       time_to_maturity = time_to_maturity,
                                       volatility = volatility,
                                       dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "asian" && tolower(model) == "jump_diffusion") {
      return(Malliavin_Asian_Greeks(payoff = payoff,
                                    greek = greek,
                                    initial_price = initial_price,
                                    exercise_price = exercise_price,
                                    r = r,
                                    time_to_maturity = time_to_maturity,
                                    volatility = volatility,
                                    dividend_yield = dividend_yield,
                                    model = model,
                                    antithetic = antithetic))
    }

    else {
      stop("Wrong input or not yet implemented.")
    }

  }
