#' Computes the implied volatility for European-, binomial- and Asian options.
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
#' @examples Binomial_American_Greeks(initial_price = 100, exercise_price = 100,
#' r = 0, time_to_maturity = 1, volatility = 0.3, dividend_yield = 0,
#' payoff = "call", greek = c("fair_value", "delta", "vega", "theta", "rho",
#' "epsilon", "gamma"), steps = 20)
#'

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

    if (option_price < BS_European_Greeks(initial_price = initial_price,
                                         exercise_price = exercise_price,
                                         r = r,
                                         time_to_maturity = time_to_maturity,
                                         volatility = 1e-9,
                                         dividend_yield = dividend_yield,
                                         payoff = payoff,
                                         greek = "fair_value")) {
      stop("Option price is too low. Implied volatility is not defined.")
    }

    if (payoff == "call") {

      volatility <- start_volatility

      while(TRUE) {

        d1 <- (log(initial_price/exercise_price) +
                 (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
          (volatility * sqrt(time_to_maturity))

        d2 <- d1 - volatility * sqrt(time_to_maturity)

        fair_value <-
          initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
          exp(-r*time_to_maturity) * exercise_price * pnorm(d2)

        #print(c(volatility, fair_value - option_price))

        if (abs(fair_value - option_price) < precision) {
          volatility <-
            volatility -
            ((fair_value - option_price) / vega)

          return(volatility)
        }

        vega <-
          initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
          sqrt(time_to_maturity)

        volatility <-
          volatility -
          ((fair_value - option_price) / vega)

      }

    }

    if (payoff == "put") {

      fair_value <- option_price + 1

      volatility <- start_volatility

      while(TRUE) {

        d1 <- (log(initial_price/exercise_price) +
                 (r - dividend_yield + (volatility^2)/2) * time_to_maturity) /
          (volatility * sqrt(time_to_maturity))

        d2 <- d1 - volatility * sqrt(time_to_maturity)

        fair_value <-
          exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
          initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1)

        #print(c(volatility, fair_value - option_price))

        if (abs(fair_value - option_price) < precision) {
          volatility <-
            volatility -
            ((fair_value - option_price) / vega)

          return(volatility)
        }

        vega <-
          initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
          sqrt(time_to_maturity)

        volatility <-
          volatility -
          ((fair_value - option_price) / vega)

      }

    }

  }
