#' @title
#' Computes the implied volatility for European put- and call options in the
#' Black Scholes model via Halley's method.
#'
#' @description For the definition of *implied volatility* see
#' [Implied_Volatility].
#' [BS_Implied_Volatility] offers a very fast implementation for European put-
#' and call options applying Halley's method (see
#'
#' [en.wikipedia.org/wiki/Halley%27s_method](https://en.wikipedia.org/wiki/Halley%27s_method)).
#'
#' @export
#'
#' @seealso [Implied_Volatility] for American and Asian options, and for
#' digital payoff functions
#'
#' @import "stats"
#' @import "Rcpp"
#'
#' @param option_price - current price of the option
#' @param initial_price - initial price of the underlying asset.
#' @param exercise_price - strike price of the option.
#' @param r - risk-free interest rate.
#' @param time_to_maturity - time to maturity.
#' @param dividend_yield - dividend yield.
#' @param payoff - the payoff function, a string in ("call", "put").
#' @param start_volatility - the volatility value to start the approximation
#' @param precision - precision of the result
#' @param max_iter - maximum number of iterations
#'
#' @useDynLib greeks, .registration=TRUE
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples BS_Implied_Volatility(option_price = 27, initial_price = 100,
#' exercise_price = 100, r = 0.03, time_to_maturity = 5, dividend_yield = 0.015,
#' payoff = "call")

BS_Implied_Volatility <-
  function(option_price,
           initial_price = 100,
           exercise_price = 100,
           r = 0,
           time_to_maturity = 1,
           dividend_yield = 0,
           payoff = "call",
           start_volatility = 0.3,
           precision = 1e-9,
           max_iter = 30) {

    if (!is.numeric(max_iter) || length(max_iter) != 1 || is.na(max_iter) ||
        !is.finite(max_iter) || max_iter < 1 ||
        max_iter > .Machine$integer.max || max_iter != floor(max_iter)) {
      stop("max_iter must be a positive integer.", call. = FALSE)
    }

    BS_Implied_Volatility_cpp(
      option_price = option_price,
      initial_price = initial_price,
      exercise_price = exercise_price,
      r = r,
      time_to_maturity = time_to_maturity,
      dividend_yield = dividend_yield,
      payoff = payoff,
      start_volatility = start_volatility,
      precision = precision,
      max_iter = as.integer(max_iter)
    )
  }
