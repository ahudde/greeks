#' @title Computes the Greeks of a European call- or put-option, or of digital
#' options in the Black Scholes model.
#'
#' @description
#' For details on the definition of Greeks see [Greeks].
#'
#' @export
#'
#' @seealso [Malliavin_European_Greeks] for the Monte Carlo implementation
#' @seealso [Greeks_UI] for an interactive visualization
#'
#' @import "stats"
#'
#' @param initial_price - initial price of the underlying asset
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param dividend_yield - dividend yield
#' @param volatility - volatility of the underlying asset
#' @param payoff - in c("call", "put", "cash_or_nothing_call",
#' "cash_or_nothing_put", "asset_or_nothing_call", "asset_or_nothing_put")
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna", "charm", "vomma",
#' "veta", "vera", "speed", "zomma", "color", "ultima")
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples BS_European_Greeks(initial_price = 120, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "gamma"), payoff = "put")

BS_European_Greeks <-
  function(initial_price = 100,
           exercise_price = 100,
           r = 0,
           time_to_maturity = 1,
           volatility = 0.3,
           dividend_yield = 0,
           payoff = "call",
           greek = c("fair_value", "delta", "vega", "theta", "rho", "epsilon",
                     "lambda", "gamma", "vanna", "charm", "vomma", "veta",
                     "speed")) {

    # Keep validation in R so public error messages stay friendly.
    positive_params <- c("initial_price", "exercise_price", "time_to_maturity",
                         "volatility")

    for (param in positive_params) {
      value <- get(param)

      if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
          !is.finite(value) || value <= 0) {
        stop(param, " must be a positive finite number.", call. = FALSE)
      }
    }

    finite_params <- c("r", "dividend_yield")

    for (param in finite_params) {
      value <- get(param)

      if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
          !is.finite(value)) {
        stop(param, " must be a finite number.", call. = FALSE)
      }
    }

    if (anyDuplicated(greek)) {
      stop("greek must not contain duplicate values.", call. = FALSE)
    }

    valid_payoffs <- c("call", "put", "cash_or_nothing_call",
                       "cash_or_nothing_put", "asset_or_nothing_call",
                       "asset_or_nothing_put")

    if (!(payoff %in% valid_payoffs)) {
      return()
    }

    BS_European_Greeks_cpp(
      initial_price = initial_price,
      exercise_price = exercise_price,
      r = r,
      time_to_maturity = time_to_maturity,
      volatility = volatility,
      dividend_yield = dividend_yield,
      payoff = payoff,
      greek = greek
    )
  }
