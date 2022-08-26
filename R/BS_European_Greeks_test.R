#' Computes the Greeks of an European call- or put-option in the Black Scholes
#' model
#'
#'
#' @import "stats"
#'
#' @param initial_price - initial price of the underlying asset
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param dividend_yield - dividend yield
#' @param volatility - volatility of the underlying asset
#' @param payoff - in "call", "put", "cash_or_nothing_call",
#' "cash_or_nothing_put", "asset_or_nothing_call", "asset_or_nothing_put"
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna", "charm", "vomma",
#' "veta", "speed")
#'
#' @return Named vector containing the values of the greeks specified in the
#' parameter \code{greek}.
#'
#' @examples BS_European_Greeks(initial_price = 120, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "gamma"), payoff = "put")

BS_European_Greeks_test <-
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

    result <- vector(mode = "numeric", length = length(greek))

    names(result) <- greek

    if (payoff == "call") {
      fair_value <-
        expression(
          initial_price * exp(-dividend_yield * time_to_maturity) *
            pnorm((log(initial_price / exercise_price) +
                     (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
                    (volatility * sqrt(time_to_maturity))) -
            exp(-r * time_to_maturity) * exercise_price *
            pnorm(((log(initial_price / exercise_price) +
                      (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
                     (volatility * sqrt(time_to_maturity)) - volatility * sqrt(time_to_maturity)))
        )
    }

    if (payoff == "put") {
      fair_value <-
        expression(
          exp(-r * time_to_maturity) * exercise_price *
            pnorm(-((log(initial_price / exercise_price) +
                       (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
                      (volatility * sqrt(time_to_maturity)) - volatility * sqrt(time_to_maturity))) -
            initial_price * exp(-dividend_yield * time_to_maturity) *
            pnorm(-(log(initial_price / exercise_price) +
                      (r - dividend_yield + (volatility^2) / 2) * time_to_maturity) /
                    (volatility * sqrt(time_to_maturity)))
        )
    }

    if (payoff == "cash_or_nothing_call") {
      fair_value <-
        expression(
          exp(-r * time_to_maturity) *
            pnorm((
              log(initial_price / exercise_price) +
                (r - dividend_yield + (volatility ^ 2) / 2) * time_to_maturity
            ) / (volatility * sqrt(time_to_maturity)) - volatility *
              sqrt(time_to_maturity)))
    }

    if (payoff == "cash_or_nothing_put") {
      fair_value <-
        expression(
          exp(-r * time_to_maturity) *
            pnorm(-(
              log(initial_price / exercise_price) +
                (r - dividend_yield + (volatility ^ 2) / 2) * time_to_maturity
            ) / (volatility * sqrt(time_to_maturity)) - volatility *
              sqrt(time_to_maturity)))
    }

    if (payoff == "asset_or_nothing_call") {
      fair_value <-
        expression(
          initial_price *
            exp(-dividend_yield * time_to_maturity) *
            pnorm(pnorm((log(initial_price / exercise_price) +
                           (r - dividend_yield + (volatility^2) / 2) *
                           time_to_maturity) /
                          (volatility * sqrt(time_to_maturity)))))
    }

    if (payoff == "asset_or_nothing_put") {
      fair_value <-
        expression(
          initial_price *
            exp(-r * time_to_maturity) *
            pnorm(-pnorm((log(initial_price / exercise_price) +
                            (r - dividend_yield + (volatility^2) / 2) *
                            time_to_maturity) /
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
    if ("dual delta" %in% greek) {
      result['dual delta'] <-
        eval(D(fair_value, "exercise_price"))
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
