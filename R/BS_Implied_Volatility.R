#' Computes the implied volatility for European options in the Black Scholes
#' model via Halley's method.
#'
#' @export
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
           precision = 1e-9) {

    ## check if option price can be obtained

    option_price_zero_vol <-
      BS_European_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = 1e-12,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = "fair_value"
      )

    if (option_price <= option_price_zero_vol) {
      stop("Option price is too low. Implied volatility is not defined.")
    }

    ## Start computation

    volatility <- start_volatility

    d1 <- (log(initial_price/exercise_price) +
             (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
      (volatility * sqrt(time_to_maturity))

    d2 <- d1 - volatility * sqrt(time_to_maturity)

    if (payoff == "call") {

      fair_value <-
        initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
        exp(-r*time_to_maturity) * exercise_price * pnorm(d2)

    } else if (payoff == "put") {

      fair_value <-
        exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
        initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1)

    }

    while (TRUE) {

      vega <-
        initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
        sqrt(time_to_maturity)

      vomma <-
        initial_price * exp(-dividend_yield * time_to_maturity) *
        dnorm(d1) * sqrt(time_to_maturity) * d1 * d2 / volatility

      volatility <-
        volatility -
        (2 * (fair_value - option_price) * vega) / (2 * vega^2 - (fair_value - option_price) * vomma)

      if (abs(fair_value - option_price) < precision) {
        return(volatility)
      }

      d1 <- (log(initial_price/exercise_price) +
               (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
        (volatility * sqrt(time_to_maturity))

      d2 <- d1 - volatility * sqrt(time_to_maturity)

      if (payoff == "call") {

        fair_value <-
          initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
          exp(-r*time_to_maturity) * exercise_price * pnorm(d2)

      } else if (payoff == "put") {

        fair_value <-
          exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
          initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1)

      }

    }

  }
