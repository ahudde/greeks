#' @title
#' Computes the Greeks of a Geometric Asian Option with classical Call- and
#' Put-Payoff in the Black Scholes model
#'
#' @description
#' For the definition of geometric Asian options see
#' [Malliavin_Geometric_Asian_Greeks].
#' [BS_Geometric_Asian_Greeks] offers a fast and exaction computation of
#' Geometric Asian Greeks.
#'
#' @export
#'
#' @seealso [Malliavin_Geometric_Asian_Greeks] for the Monte Carlo
#' implementation which provides digital and custom payoff functions and also
#' works for the jump diffusion model
#' @seealso [Greeks_UI] for an interactive visualization
#'
#' @import "stats"
#' @import "Rcpp"
#' @importFrom "dqrng" "dqrnorm" "dqset.seed"
#'
#' @param initial_price - initial price of the underlying asset, can also be a
#' vector
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param volatility - volatility of the underlying asset
#' @param dividend_yield - dividend yield
#' @param payoff - the payoff function, either a string in ("call", "put")
#' @param greek - the Greeks to be calculated in c("fair_value", "delta",
#' "vega", "theta", "rho", "gamma", "vomma")
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples BS_Geometric_Asian_Greeks(initial_price = 110, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "rho", "vega", "theta", "gamma"),
#' payoff = "put")
#'

BS_Geometric_Asian_Greeks <- function(
    initial_price = 100,
    exercise_price = 100,
    r = 0,
    time_to_maturity = 1,
    volatility = 0.3,
    dividend_yield = 0,
    payoff = "call",
    greek = c("fair_value", "delta", "rho", "vega", "theta", "gamma")) {

  result <- numeric(length(greek)) * NA

  names(result) = greek

  d_geom <-
    (log(initial_price/exercise_price)
     + (time_to_maturity/2) * ((r - dividend_yield) - (volatility**2)/2)) /
    (volatility * sqrt(time_to_maturity/3))

  if ("delta" %in% greek || "gamma" %in% greek) {
    # the derivative of d_geom with respect to initial_price
    d_delta <-
      sqrt(3) / (initial_price * volatility * sqrt(time_to_maturity))
  } #d_delta

  if ("vega" %in% greek || "vomma" %in% greek) {
    # the derivative of d_geom with respect to volatility
    d_vega <-
      -sqrt(3 * time_to_maturity) *
      (0.25 +
         (log(initial_price/exercise_price) / (volatility^2 * time_to_maturity)) +
         ((r - dividend_yield) / (2 * volatility^2)))
  } #d_vega

  if ("theta" %in% greek) {
    # the derivative of d_geom with respect to time_to_maturity
    d_theta <-
      -sqrt(3) * log(initial_price/exercise_price) /
      (2*volatility*(time_to_maturity**1.5)) +
      sqrt(3) / (4 * volatility * sqrt(time_to_maturity)) *
      ((r - dividend_yield) - (volatility**2)/2)
  } #d_theta

  if ("gamma" %in% greek) {
    # the derivative of d_geom with respect to x^2
    d_gamma <-
      -sqrt(3) / (initial_price^2 * volatility * sqrt(time_to_maturity))
  } #d_gamma

  if (payoff == "call") {

    if ("fair_value" %in% greek) {

      result["fair_value"] <-
        initial_price *
        exp(-time_to_maturity/2 * ((r - dividend_yield) + (volatility**2)/6)) *
        pnorm(d_geom + volatility * sqrt(time_to_maturity/3)) -
        (exp(-(r - dividend_yield) * time_to_maturity) *
           exercise_price * pnorm(d_geom))
    } #fair_value

    if ("delta" %in% greek) {

      result["delta"] <-
        exp(-(time_to_maturity/2) * ((r - dividend_yield) + (volatility**2)/6)) *
        (pnorm(d_geom + (volatility * sqrt(time_to_maturity/3))) +
           initial_price *
           dnorm(d_geom + (volatility*sqrt(time_to_maturity/3))) * d_delta) -
        (exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
           dnorm(d_geom) * d_delta)
    } #delta

    if ("vega" %in% greek) {

      result["vega"] <-
        initial_price * exp(-(time_to_maturity/2) *
                              ((r - dividend_yield) + (volatility**2)/6)) *
        ((-volatility * time_to_maturity/6) *
           pnorm(d_geom + volatility * sqrt(time_to_maturity/3)) +
           dnorm(d_geom + volatility * sqrt(time_to_maturity/3)) *
           (d_vega + sqrt(time_to_maturity/3))) -
        exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
        dnorm(d_geom) * d_vega
    } #vega

    if ("rho" %in% greek) {

      result["rho"] <-
        initial_price *
        exp(-time_to_maturity/2 * ((r - dividend_yield) + (volatility**2)/6)) *
        ((-time_to_maturity/2) *
           pnorm(d_geom + volatility * sqrt(time_to_maturity/3)) +
           dnorm(d_geom + volatility * sqrt(time_to_maturity/3)) *
           (sqrt(3 * time_to_maturity)/(2 * volatility))) +
        exercise_price * exp(-(r - dividend_yield) * time_to_maturity) *
        (
          time_to_maturity * pnorm(d_geom) -
            dnorm(d_geom) * sqrt(3 * time_to_maturity)/(2 * volatility)
        )
    } # rho

    if ("theta" %in% greek) {

      result["theta"] <-
        -initial_price * exp(-time_to_maturity/2 *
                               ((r - dividend_yield) + (volatility**2)/6)) *
        (-0.5 * ((r - dividend_yield) + (volatility**2)/6) *
           pnorm(d_geom + volatility * sqrt(time_to_maturity/3)) +
           dnorm(d_geom + volatility * sqrt(time_to_maturity/3)) *
           (d_theta +
              volatility/(2 * sqrt(3*time_to_maturity)))) +
        exercise_price * exp(-(r - dividend_yield)*time_to_maturity) *
        (-(r - dividend_yield) * pnorm(d_geom) +
           dnorm(d_geom) * d_theta)
    } #theta

    if ("gamma" %in% greek) {

      result["gamma"] <-
        exp(-(time_to_maturity/2) * ((r - dividend_yield) + (volatility**2)/6)) *
        dnorm(d_geom + (volatility * sqrt(time_to_maturity/3))) *
        (2 * d_delta - initial_price *
           (d_geom + volatility * sqrt(time_to_maturity/3)) * d_delta**2 +
           initial_price * d_gamma) +
        (exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
           dnorm(d_geom) * (d_geom * d_delta**2 - d_gamma))
    }

    if("vomma" %in% greek) {
      result["vomma"] <-
        eval(D(expression(initial_price * exp(-(time_to_maturity/2) *
                                                ((r - dividend_yield) + (volatility**2)/6)) *
                            ((-volatility * time_to_maturity/6) *
                               pnorm(d_geom + volatility * sqrt(time_to_maturity/3)) +
                               dnorm(d_geom + volatility * sqrt(time_to_maturity/3)) *
                               (d_vega + sqrt(time_to_maturity/3))) -
                            exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
                            dnorm(d_geom) * d_vega), "volatility"))
    }

  } #payoff=="call"


  if (payoff == "put") {

    if ("fair_value" %in% greek) {

      result["fair_value"] <-
        -initial_price *
        exp(-(time_to_maturity/2) * ((r - dividend_yield) + (volatility**2)/6)) *
        pnorm(-d_geom - (volatility*sqrt(time_to_maturity) / sqrt(3))) +
        exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
        pnorm(-d_geom)
    } #fair_value

    if ("delta" %in% greek) {

      result["delta"] <-
        -exp(-(time_to_maturity/2) * ((r - dividend_yield) + (volatility**2)/6)) *
        (pnorm(-d_geom - (volatility * sqrt(time_to_maturity/3))) -
           initial_price *
           dnorm(-d_geom - (volatility*sqrt(time_to_maturity/3))) * d_delta) -
        (exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
           dnorm(-d_geom) * d_delta)
    } #delta

    if ("vega" %in% greek) {

      result["vega"] <-
        -initial_price * exp(-(time_to_maturity/2) *
                               ((r - dividend_yield) + (volatility**2)/6)) *
        ((-volatility * time_to_maturity/6) *
           pnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) -
           dnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) *
           (d_vega + sqrt(time_to_maturity/3))) -
        exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
        dnorm(-d_geom) * d_vega
    } #vega

    if ("rho" %in% greek) {

      result["rho"] <-
        -initial_price *
        exp(-time_to_maturity/2 * ((r - dividend_yield) + (volatility**2)/6)) *
        ((-time_to_maturity/2) *
           pnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) -
           dnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) *
           (sqrt(3 * time_to_maturity)/(2 * volatility))) -
        exercise_price * exp(-(r - dividend_yield) * time_to_maturity) *
        (
          time_to_maturity * pnorm(-d_geom) +
            dnorm(-d_geom) * sqrt(3 * time_to_maturity)/(2 * volatility)
        )
    } # rho

    if ("theta" %in% greek) {

      result["theta"] <-
        initial_price * exp(-time_to_maturity/2 *
                              ((r - dividend_yield) + (volatility**2)/6)) *
        (-0.5 * ((r - dividend_yield) + (volatility**2)/6) *
           pnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) -
           dnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) *
           (d_theta +
              volatility/(2 * sqrt(3*time_to_maturity)))) +
        exercise_price * exp(-(r - dividend_yield)*time_to_maturity) *
        ((r - dividend_yield) * pnorm(-d_geom) +
           dnorm(-d_geom) * d_theta)
    } #theta

    if ("gamma" %in% greek) {

      result["gamma"] <-
        exp(-(time_to_maturity/2) * ((r - dividend_yield) + (volatility**2)/6)) *
        dnorm(-d_geom - (volatility * sqrt(time_to_maturity/3))) *
        (2 * d_delta - initial_price *
           (-d_geom - volatility * sqrt(time_to_maturity/3)) * d_delta**2 +
           initial_price * d_gamma) -
        (exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
           dnorm(-d_geom) * (d_geom * d_delta**2 - d_gamma))
    }

    if("vomma" %in% greek) {
      result["vomma"] <-
        eval(D(expression(
          -initial_price * exp(-(time_to_maturity/2) *
                                 ((r - dividend_yield) + (volatility**2)/6)) *
            ((-volatility * time_to_maturity/6) *
               pnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) -
               dnorm(-d_geom - volatility * sqrt(time_to_maturity/3)) *
               (d_vega + sqrt(time_to_maturity/3))) -
            exp(-(r - dividend_yield)*time_to_maturity) * exercise_price *
            dnorm(-d_geom) * d_vega), "volatility"))
    }

  } #payoff=="put"

  return(result)
}
