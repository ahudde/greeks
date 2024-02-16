#' Computes the Greeks of a European call- or put-option, or of digital options
#' in the Black Scholes model
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

    result <- vector(mode = "numeric", length = length(greek)) * NA

    names(result) <- greek

    sqrt_time_to_maturity <- sqrt(time_to_maturity)

    d1 <- (log(initial_price / exercise_price) +
             (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
      (volatility * sqrt_time_to_maturity)

    d2 <- d1 - volatility * sqrt_time_to_maturity

    exp_minus_dividend_yield_times_time_to_maturity <-
      exp(-dividend_yield * time_to_maturity)

    exp_minus_r_times_time_to_maturity <- exp(-r * time_to_maturity)

    pnorm_d1 <- pnorm(d1)

    dnorm_d1 <- dnorm(d1)

    pnorm_d2 <- pnorm(d2)

    if (payoff == "call") {

      ## option-value

      if ("fair_value" %in% greek) {
        result['fair_value'] <-
          initial_price * exp_minus_dividend_yield_times_time_to_maturity *
          pnorm_d1 - exp_minus_r_times_time_to_maturity * exercise_price *
          pnorm_d2
      }

      ## first-order Greeks

      if ("delta" %in% greek) {
        result['delta'] <- exp_minus_dividend_yield_times_time_to_maturity *
          pnorm_d1
      }
      if ("theta" %in% greek) {
        result['theta'] <-
          -initial_price * dnorm_d1 * volatility *
          exp_minus_dividend_yield_times_time_to_maturity /
          (2 * sqrt_time_to_maturity) + dividend_yield * initial_price *
          pnorm_d1 * exp_minus_dividend_yield_times_time_to_maturity -
          r * exercise_price * exp_minus_r_times_time_to_maturity * pnorm_d2
      }
      if ("rho" %in% greek) {
        result['rho'] <-
          exercise_price * time_to_maturity *
          exp_minus_r_times_time_to_maturity * pnorm_d2
      }
      if ("epsilon" %in% greek) {
        result['epsilon'] <-
          -initial_price * time_to_maturity *
          exp_minus_dividend_yield_times_time_to_maturity * pnorm_d1
      }
      if ("lambda" %in% greek) {
        result['lambda'] <-
          initial_price *
          (exp_minus_dividend_yield_times_time_to_maturity * pnorm_d1) /
          (initial_price * exp_minus_dividend_yield_times_time_to_maturity
           * pnorm_d1 - exp_minus_r_times_time_to_maturity * exercise_price *
             pnorm_d2)
      }

      ## second-order Greeks

      if ("charm" %in% greek) {
        result["charm"] <-
          (dividend_yield * exp_minus_dividend_yield_times_time_to_maturity *
             pnorm_d1) -
          (exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
             (2 * (r - dividend_yield) * time_to_maturity -
                d2 * volatility * sqrt_time_to_maturity) /
             (2 * time_to_maturity * volatility * sqrt_time_to_maturity))
      }
    }

    if (payoff == "put") {

      pnorm_minus_d2 <- pnorm(-d2)

      dnorm_minus_d2 <- dnorm(-d2)

      pnorm_minus_d1 <- pnorm(-d1)

      ## option-value

      if ("fair_value" %in% greek) {
        result['fair_value'] <-
          exp_minus_r_times_time_to_maturity * exercise_price * pnorm_minus_d2 -
          initial_price * exp_minus_dividend_yield_times_time_to_maturity *
          pnorm_minus_d1
      }

      ## first-order Greeks

      if ("delta" %in% greek) {
        result['delta'] <-
          -exp_minus_dividend_yield_times_time_to_maturity * pnorm_minus_d1
      }
      if ("theta" %in% greek) {
        result['theta'] <-
          -initial_price * dnorm_d1 * volatility *
          exp_minus_dividend_yield_times_time_to_maturity /
          (2 * sqrt_time_to_maturity) - dividend_yield * initial_price *
          pnorm_minus_d1 * exp_minus_dividend_yield_times_time_to_maturity +
          r * exercise_price * exp_minus_r_times_time_to_maturity *
          pnorm_minus_d2
      }
      if ("rho" %in% greek) {
        result['rho'] <-
          -exercise_price * time_to_maturity *
          exp_minus_r_times_time_to_maturity * pnorm_minus_d2
      }
      if ("epsilon" %in% greek) {
        result['epsilon'] <-
          initial_price * time_to_maturity *
          exp_minus_dividend_yield_times_time_to_maturity * pnorm_minus_d1
      }
      if ("lambda" %in% greek) {
        result['lambda'] <-
          -initial_price *
          (exp_minus_dividend_yield_times_time_to_maturity * pnorm_minus_d1) /
          (exp_minus_r_times_time_to_maturity * exercise_price *
             pnorm_minus_d2 - initial_price *
             exp_minus_dividend_yield_times_time_to_maturity * pnorm_minus_d1)
      }

      # second-order Greeks

      if ("charm" %in% greek) {
        result["charm"] <-
          (-dividend_yield * exp_minus_dividend_yield_times_time_to_maturity *
             pnorm_minus_d1) -
          (exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
             (2 * (r - dividend_yield) * time_to_maturity -
                d2 * volatility * sqrt_time_to_maturity) /
             (2 * time_to_maturity * volatility * sqrt_time_to_maturity))
      }
    }

    ## The case, where call- and put Greeks have the same values

    if (payoff %in% c("call", "put")) {

      ## first-order Greeks

      if ("vega" %in% greek) {
        result['vega'] <-
          initial_price * exp_minus_dividend_yield_times_time_to_maturity *
          dnorm_d1 * sqrt_time_to_maturity
      }

      ## second-order Greeks

      if ("gamma" %in% greek) {
        result['gamma'] <-
          dnorm_d1 * exp_minus_dividend_yield_times_time_to_maturity /
          (initial_price * volatility * sqrt_time_to_maturity)
      }
      if ("vanna" %in% greek) {
        result['vanna'] <-
          -exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 * d2 /
          volatility
      }
      if ("vomma" %in% greek) {
        result["vomma"] <-
          initial_price * exp_minus_dividend_yield_times_time_to_maturity *
          dnorm_d1 * sqrt_time_to_maturity * d1 * d2 / volatility
      }
      if ("veta" %in% greek) {
        result["veta"] <-
          -initial_price * exp_minus_dividend_yield_times_time_to_maturity *
          dnorm_d1 * sqrt_time_to_maturity *
          (dividend_yield + ((r - dividend_yield) * d1) /
             (volatility * sqrt_time_to_maturity)
           - ((1 + d1 * d2) / (2 * time_to_maturity)))
      }
      if ("vera" %in% greek) {
        result["vera"] <-
          exercise_price * time_to_maturity *
          exp_minus_r_times_time_to_maturity * dnorm(d2) *
          -(sqrt_time_to_maturity + d2 / volatility)
      }

      # third-order Greeks

      if ("speed" %in% greek) {
        result["speed"] <-
          -exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 /
          (initial_price^2  * volatility * sqrt_time_to_maturity) *
          (d1 / (volatility * sqrt_time_to_maturity) + 1)
      }
      if ("zomma" %in% greek) {
        result["zomma"] <-
          exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
          (d1 * d2 - 1) / (initial_price * volatility^2 * sqrt_time_to_maturity)
      }
      if ("color" %in% greek) {
        result["color"] <-
          -exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 /
          (2 * initial_price * time_to_maturity^1.5 * volatility) *
          (2 * dividend_yield * time_to_maturity + 1 +
             (2 * (r - dividend_yield) * time_to_maturity -
                d2 * volatility * sqrt_time_to_maturity) /
             (volatility * sqrt_time_to_maturity) * d1)
      }
      if ("ultima" %in% greek) {
        result["ultima"] <-
          (-initial_price * exp_minus_dividend_yield_times_time_to_maturity *
             dnorm_d1 * sqrt_time_to_maturity) / volatility^2 *
          (d1 * d2 * (1 - d1 * d2) + d1^2 + d2^2)
      }

      return(result)

    }

    if (payoff %in% c("cash_or_nothing_call", "cash_or_nothing_put",
                      "asset_or_nothing_call", "asset_or_nothing_put")) {

      if (payoff == "cash_or_nothing_call") {
        fair_value <-
          expression(
            exp(-r * time_to_maturity) *
              pnorm((
                log(initial_price / exercise_price) +
                  (r - dividend_yield + (volatility ^ 2) / 2) * time_to_maturity
              ) / (volatility * sqrt(time_to_maturity)) -
                volatility * sqrt(time_to_maturity)))
      }

      if (payoff == "cash_or_nothing_put") {
        fair_value <-
          expression(
            exp(-r * time_to_maturity) *
              pnorm(-((
                log(initial_price / exercise_price) +
                  (r - dividend_yield + (volatility ^ 2) / 2) * time_to_maturity
              ) / (volatility * sqrt(time_to_maturity)) -
                volatility * sqrt(time_to_maturity))))
      }

      if (payoff == "asset_or_nothing_call") {
        fair_value <-
          expression(
            initial_price *
              exp(-dividend_yield * time_to_maturity) *
              pnorm(
                (log(initial_price / exercise_price) +
                   (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
                  (volatility * sqrt(time_to_maturity))))
      }

      if (payoff == "asset_or_nothing_put") {
        fair_value <-
          expression(
            initial_price *
              exp(-dividend_yield * time_to_maturity) *
              pnorm(-(
                (log(initial_price / exercise_price) +
                   (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
                  (volatility * sqrt(time_to_maturity)))))
      }

      ## option-value

      if ("fair_value" %in% greek) {
        result['fair_value'] <-
          eval(fair_value)
      }

      ## first-order Greeks

      if ("delta" %in% greek) {
        result['delta'] <-
          eval(D(fair_value, "initial_price"))
      }
      if ("vega" %in% greek) {
        result['vega'] <-
          eval(D(fair_value, "volatility"))
      }
      if ("theta" %in% greek) {
        result['theta'] <-
          -eval(D(fair_value, "time_to_maturity"))
      }
      if ("rho" %in% greek) {
        result['rho'] <-
          eval(D(fair_value, "r"))
      }
      if ("epsilon" %in% greek) {
        result['epsilon'] <-
          eval(D(fair_value, "dividend_yield"))
      }
      if ("lambda" %in% greek) {
        result['lambda'] <-
          eval(D(fair_value, "initial_price")) * initial_price /
          eval(fair_value)
      }

      # second-order Greeks

      if ("gamma" %in% greek) {
        result['gamma'] <-
          eval(D(D(fair_value, "initial_price"), "initial_price"))
      }
      if ("vanna" %in% greek) {
        result['vanna'] <-
          eval(D(D(fair_value, "volatility"), "initial_price"))
      }
      if ("charm" %in% greek) {
        result["charm"] <-
          -eval(D(D(fair_value, "time_to_maturity"), "initial_price"))
      }
      if ("vomma" %in% greek) {
        result["vomma"] <-
          eval(D(D(fair_value, "volatility"), "volatility"))
      }
      if ("veta" %in% greek) {
        result["veta"] <-
          eval(D(D(fair_value, "time_to_maturity"), "volatility"))
      }
      if ("vera" %in% greek) {
        result["vera"] <-
          eval(D(D(fair_value, "r"), "volatility"))
      }

      # third-order Greeks

      if ("speed" %in% greek) {
        result["speed"] <-
          eval(D(D(D(fair_value, "initial_price"), "initial_price"),
                 "initial_price"))
      }
      if ("zomma" %in% greek) {
        result["zomma"] <-
          eval(D(D(D(fair_value, "volatility"), "initial_price"),
                 "initial_price"))
      }
      if ("color" %in% greek) {
        result["color"] <-
          eval(D(D(D(fair_value, "time_to_maturity"), "initial_price"),
                 "initial_price"))
      }
      if ("ultima" %in% greek) {
        result["ultima"] <-
          eval(D(D(D(fair_value, "volatility"), "volatility"), "volatility"))
      }

      return(result)

    }

  }
