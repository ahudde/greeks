#' Computes the Greeks of an American call- or put-option with the Binomial
#' options pricing model
#'
#' @export
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
           eps = 1/10000) {

  result <- numeric(length(greek))

  names(result) <- greek

  v <- Binomial_American_Greeks_cpp(initial_price = initial_price,
                                    exercise_price = exercise_price,
                                    r = r,
                                    time_to_maturity = time_to_maturity,
                                    volatility = volatility,
                                    dividend_yield = dividend_yield,
                                    payoff = payoff,
                                    steps = steps)

  if ("fair_value" %in% greek) {
    result["fair_value"] <- v["fair_value"]
  }

  if ("delta" %in% greek) {
    result["delta"] <- v["delta"]
  }

  if ("gamma" %in% greek) {
    result["gamma"] <- v["gamma"]
  }

  if ("vega" %in% greek) {
    v_up <- Binomial_American_Greeks_cpp(initial_price = initial_price,
                                 exercise_price = exercise_price,
                                 r = r,
                                 time_to_maturity = time_to_maturity,
                                 volatility = volatility + eps,
                                 dividend_yield = dividend_yield,
                                 payoff = payoff,
                                 steps = steps)["fair_value"]
    v_down <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                        exercise_price = exercise_price,
                                        r = r,
                                        time_to_maturity = time_to_maturity,
                                        volatility = volatility - eps,
                                        dividend_yield = dividend_yield,
                                        payoff = payoff,
                                        steps = steps)["fair_value"]

    result["vega"] <- (v_up - v_down) / (2*eps)
  }

  if ("theta" %in% greek) {
    v_up <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                   exercise_price = exercise_price,
                                   r = r,
                                   time_to_maturity = time_to_maturity + eps,
                                   volatility = volatility,
                                   dividend_yield = dividend_yield,
                                   payoff = payoff,
                                   steps = steps)["fair_value"]
    v_down <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                   exercise_price = exercise_price,
                                   r = r,
                                   time_to_maturity = time_to_maturity - eps,
                                   volatility = volatility,
                                   dividend_yield = dividend_yield,
                                   payoff = payoff,
                                   steps = steps)["fair_value"]

    result["theta"] <- - (v_up - v_down) / (2*eps)
  }

  if ("rho" %in% greek) {
    v_up <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                   exercise_price = exercise_price,
                                   r = r + eps,
                                   time_to_maturity = time_to_maturity,
                                   volatility = volatility,
                                   dividend_yield = dividend_yield,
                                   payoff = payoff,
                                   steps = steps)["fair_value"]
    v_down <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                   exercise_price = exercise_price,
                                   r = r - eps,
                                   time_to_maturity = time_to_maturity,
                                   volatility = volatility,
                                   dividend_yield = dividend_yield,
                                   payoff = payoff,
                                   steps = steps)["fair_value"]

    result["rho"] <- (v_up - v_down) / (2*eps)
  }

  if ("epsilon" %in% greek) {
    v_up <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                   exercise_price = exercise_price,
                                   r = r,
                                   time_to_maturity = time_to_maturity,
                                   volatility = volatility,
                                   dividend_yield = dividend_yield + eps,
                                   payoff = payoff,
                                   steps = steps)["fair_value"]
    v_down <-
      Binomial_American_Greeks_cpp(initial_price = initial_price,
                                   exercise_price = exercise_price,
                                   r = r,
                                   time_to_maturity = time_to_maturity,
                                   volatility = volatility,
                                   dividend_yield = dividend_yield - eps,
                                   payoff = payoff,
                                   steps = steps)["fair_value"]

    result["epsilon"] <- (v_up - v_down) / (2*eps)
  }

  return(result)

}
