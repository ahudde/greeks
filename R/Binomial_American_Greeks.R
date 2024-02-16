#' Computes the Greeks of an American call- or put-option with the Binomial
#' options pricing model
#'
#' @export
#'
#' @seealso [Greeks_UI] for an interactive visualization
#'
#' @import "stats"
#' @import "Rcpp"
#' @importFrom "dqrng" "dqrnorm" "dqset.seed"
#'
#' @param initial_price - initial price of the underlying asset.
#' @param exercise_price - strike price of the option.
#' @param r - risk-free interest rate.
#' @param time_to_maturity - time to maturity.
#' @param volatility - volatility of the underlying asset.
#' @param dividend_yield - dividend yield.
#' @param payoff - the payoff function, a string in ("call", "put").
#' @param greek - the Greek to be calculated.
#' @param steps - the number of integration steps.
#' @param eps - the step size for the finite difference method to calculate
#' theta, vega, rho and epsilon
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

Binomial_American_Greeks <-
  function(initial_price = 100,
           exercise_price = 100,
           r = 0,
           time_to_maturity = 1,
           volatility = 0.3,
           dividend_yield = 0,
           payoff = "call",
           greek = c("fair_value", "delta", "vega", "theta", "rho", "epsilon",
                     "gamma"),
           steps = 1000,
           eps = 1/100000) {

    ## Definition of a helper-function of make better use of
    ## Binomial_American_Greeks.cpp

    compute_fair_value <- function(...) {

      if (length(list(...)) >= 1) {
        for (i in 1:length(list(...))) {
          param_name <- names(list(...)[i])
          param_value <- list(...)[[i]]
          assign(x = param_name, value = param_value)
        }
      }

      v <- Binomial_American_Greeks_cpp(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        steps = steps)

      return(
        v["fair_value"] +
          BS_European_Greeks(
            initial_price = initial_price,
            exercise_price = exercise_price,
            r = r,
            time_to_maturity = time_to_maturity,
            volatility = volatility,
            dividend_yield = dividend_yield,
            payoff = payoff,
            greek = "fair_value") -
          v["european_fair_value"]
      )
    }

    ## the actual function definition
    result <- numeric(length(greek)) * NA

    names(result) <- greek

    if ("fair_value" %in% greek) {
      result["fair_value"] <- compute_fair_value()
    }

    if ("delta" %in% greek) {

      v_up <- compute_fair_value(initial_price = initial_price + eps)

      v_down <- compute_fair_value(initial_price = initial_price - eps)

      result["delta"] <- (v_up - v_down) / (2*eps)

    }

    if ("gamma" %in% greek) {

      eps_gamma <- initial_price/50

      v_up <- compute_fair_value(initial_price = initial_price + eps_gamma)
      v <- compute_fair_value(initial_price = initial_price)
      v_down <- compute_fair_value(initial_price = initial_price - eps_gamma)

      result["gamma"] <-
        (v_up - 2*v + v_down) / (eps_gamma**2)

    }

    if ("vega" %in% greek) {
      v_up <- compute_fair_value(volatility = volatility + eps)

      v_down <- compute_fair_value(volatility = volatility - eps)

      result["vega"] <- (v_up - v_down) / (2*eps)
    }

    if ("theta" %in% greek) {
      v_up <- compute_fair_value(time_to_maturity = time_to_maturity + eps)

      v_down <- compute_fair_value(time_to_maturity = time_to_maturity - eps)

      result["theta"] <- - (v_up - v_down) / (2*eps)
    }

    if ("rho" %in% greek) {
      v_up <- compute_fair_value(r = r + eps)

      v_down <- compute_fair_value(r = r - eps)

      result["rho"] <- (v_up - v_down) / (2*eps)
    }

    if ("epsilon" %in% greek) {
      v_up <- compute_fair_value(dividend_yield = dividend_yield + eps)

      v_down <- compute_fair_value(dividend_yield = dividend_yield - eps)

      result["epsilon"] <- (v_up - v_down) / (2*eps)
    }

    return(result)

  }
