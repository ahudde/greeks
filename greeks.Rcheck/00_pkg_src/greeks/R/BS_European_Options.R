#' Calculates the greeks of an European call- or put-option in the Black Scholes
#' model.
#'
#' @export
#'
#' @import "stats"
#'
#' @param initial_price - initial price of the underlying asset
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity
#' @param dividend_yield - dividend yield
#' @param volatility - volatility of the underlying asset
#' @param payoff - in c("call", "put")
#' @param greek - greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna")
#'
#' @return Named vector containing the values of the greeks specified in the
#' parameter \code{greek}.
#'
#' @examples BS_European_Greeks(initial_price = 120, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "gamma"), payoff = "put")

BS_European_Greeks <- function(initial_price = 100,
                               exercise_price = 100,
                               r = 0,
                               time_to_maturity = 1,
                               volatility = 0.3,
                               dividend_yield = 0,
                               payoff = "call",
                               greek = c("fair_value", "delta", "vega", "theta",
                                         "rho", "epsilon", "lambda", "gamma",
                                         "vanna")) {

  result <- vector(mode = "numeric", length = length(greek))

  names(result) <- greek

  d1 <- (log(initial_price/exercise_price) +
           (r - dividend_yield + (volatility^2)/2) * time_to_maturity) / (volatility * sqrt(time_to_maturity))

  d2 <- d1 - volatility * sqrt(time_to_maturity)

  if(payoff == "call") {

    ## option-value

    if("fair_value" %in% greek) {
      result['fair_value'] <-
        initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
        exp(-r*time_to_maturity) * exercise_price * pnorm(d2)
    }

    ## first-order Greeks

    if("delta" %in% greek) {
      result['delta'] <- exp(-dividend_yield*time_to_maturity) * pnorm(d1)
    }
    if("vega" %in% greek) {
      result['vega'] <-
        initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
        sqrt(time_to_maturity)
    }
    if("theta" %in% greek) {
      result['theta'] <-
       -initial_price*dnorm(d1)*volatility*
        exp(-dividend_yield*time_to_maturity)/(2*sqrt(time_to_maturity)) +
        dividend_yield*initial_price*pnorm(d1)*
        exp(-dividend_yield*time_to_maturity) -
        r*exercise_price*exp(-r*time_to_maturity)*pnorm(d2)
    }
    if("rho" %in% greek) {
      result['rho'] <-
        exercise_price*time_to_maturity*exp(-r*time_to_maturity)*pnorm(d2)
    }
    if("epsilon" %in% greek) {
      result['epsilon'] <-
        -initial_price*time_to_maturity*exp(-dividend_yield*time_to_maturity)*
        pnorm(d1)
    }
    if("lambda" %in% greek) {
      result['lambda'] <-
        initial_price * (exp(-dividend_yield*time_to_maturity) * pnorm(d1)) /
        (initial_price * exp(-dividend_yield*time_to_maturity) * pnorm(d1) -
        exp(-r*time_to_maturity) * exercise_price * pnorm(d2))
    }

    ## second-order Greeks

    if("gamma" %in% greek) {
      result['gamma'] <-
        dnorm(d1)*exp(-dividend_yield*time_to_maturity)/
        (initial_price*volatility*sqrt(time_to_maturity))
    }
    if("vanna" %in% greek) {
      result['vanna'] <-
        -exp(-dividend_yield*time_to_maturity)*dnorm(d1)*d2/volatility
    }

  }
  if(payoff == "put") {

    ## option-value

    if("fair_value" %in% greek) {
      result['fair_value'] <-
        exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
        initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1)
    }

    ## first-order Greeks

    if("delta" %in% greek) {
      result['delta'] <- -exp(-dividend_yield*time_to_maturity) * pnorm(-d1)
    }
    if("vega" %in% greek) {
      result['vega'] <-
        initial_price*exp(-dividend_yield*time_to_maturity) * dnorm(d1) *
        sqrt(time_to_maturity)
    }
    if("theta" %in% greek) {
      result['theta'] <-
        -initial_price*dnorm(d1)*volatility*
        exp(-dividend_yield*time_to_maturity)/(2*sqrt(time_to_maturity)) -
        dividend_yield*initial_price*pnorm(-d1)*
        exp(-dividend_yield*time_to_maturity) +
        r*exercise_price*exp(-r*time_to_maturity)*pnorm(-d2)
    }
    if("rho" %in% greek) {
      result['rho'] <-
        -exercise_price*time_to_maturity*exp(-r*time_to_maturity)*pnorm(-d2)
    }
    if("epsilon" %in% greek) {
      result['epsilon'] <-
        initial_price*time_to_maturity*exp(-dividend_yield*time_to_maturity)*
        pnorm(-d1)
    }
    if("lambda" %in% greek) {
      result['lambda'] <-
        initial_price * (exp(-dividend_yield*time_to_maturity) * pnorm(d1)) /
        (exp(-r*time_to_maturity) * exercise_price * pnorm(-d2) -
           initial_price * exp(-dividend_yield * time_to_maturity) * pnorm(-d1))
    }

    # second-order Greeks

    if("gamma" %in% greek) {
      result['gamma'] <-
        dnorm(d1)*exp(-dividend_yield*time_to_maturity)/
        (initial_price*volatility*sqrt(time_to_maturity))
    }
    if("vanna" %in% greek) {
      result['vanna'] <-
        -exp(-dividend_yield*time_to_maturity)*dnorm(d1)*d2/volatility
    }


  }

  return(result)
}
