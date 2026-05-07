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
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "rho",
#' "vega", "theta", "gamma", "vomma")
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
    greek = c("fair_value", "delta", "rho", "vega", "theta", "gamma", "vomma")) {

  BS_Geometric_Asian_Greeks_cpp(
    # Keep the old scalar-return behavior for vector numeric inputs until the
    # validation pass handles them intentionally.
    initial_price = initial_price[1],
    exercise_price = exercise_price[1],
    r = r[1],
    time_to_maturity = time_to_maturity[1],
    volatility = volatility[1],
    dividend_yield = dividend_yield[1],
    payoff = payoff,
    greek = greek
  )
}
