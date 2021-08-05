#' Computes the Greeks of an European option with the Malliavin Monte Carlo
#' Method in the Black Scholes model
#'
#' @export
#'
#' @import "stats"
#'
#' @param initial_price - initial price of the underlying asset.
#' @param exercise_price - strike price of the option.
#' @param r - risk-free interest rate.
#' @param time_to_maturity - time to maturity.
#' @param volatility - volatility of the underlying asset.
#' @param dividend_yield - dividend yield.
#' @param payoff - the payoff function, either a string in ("call", "put",
#' "digital_call", "digital_put"), or a function.
#' @param greek - the greek to be calculated.
#' @param model - the model to be chosen.
#' @param paths - the number of simulated paths.
#' @param seed - the seed of the random number generator
#' @param antithetic - if TRUE, antithetic random numbers will be chosen to
#' decrease variance
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples Malliavin_European_Greeks(initial_price = 110, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "rho"), payoff = "put")
#'

Malliavin_European_Greeks <- function(initial_price = 100,
                                   exercise_price = 100,
                                   r = 0,
                                   time_to_maturity = 1,
                                   volatility = 0.3,
                                   dividend_yield = 0,
                                   payoff = "call",
                                   greek = c("fair_value", "delta", "vega",
                                             "theta", "rho", "gamma"),
                                   model = "Black Scholes",
                                   paths = 10000,
                                   seed = 1,
                                   antithetic = FALSE) {

  result <- vector(mode = "numeric", length = length(greek))

  names(result) <- greek

  ## the seed is set locally

  if(!is.na(seed)) {
    set.seed(seed)
  }

  ## the increments of the Brownian motion ###

  if(antithetic == TRUE) {
    W_T <- rnorm(n = paths/2, sd = sqrt(time_to_maturity))
    W_T <- rbind(W_T, -W_T)
  } else {
    W_T <- rnorm(n = paths, sd = sqrt(time_to_maturity))
  }

  ### the payoff function ###

  if(class(payoff) == "function") {
    print("custom payoff")
  } else if(payoff == "call") {
    payoff <- function(x) {
      return(pmax(0, x-exercise_price))
    }
  } else if(payoff == "put") {
    payoff <- function(x) {
      return(pmax(0, exercise_price-x))
    }
  } else if(payoff == "digital_call") {
    payoff <- function(x) {ifelse(x >= exercise_price, 1, 0)
    }
  } else if(payoff == "digital_put") {
    payoff <- function(x) {ifelse(x <= exercise_price, 1, 0)
    }
  }

  if(model == "Black Scholes") {
    X_T <- initial_price *
      exp((r-(volatility^2)/2)*time_to_maturity + (volatility*W_T))
    } else {
    print("Unknown model")
    return()
  }

  E <- function(weight) {
    return(exp(-r*time_to_maturity) * mean(payoff(X_T) * weight))
  }

  if("fair_value" %in% greek) {
    result["fair_value"] <-
      E(1)
  }

  if("delta" %in% greek) {
    result["delta"] <-
      (W_T / (initial_price * volatility * time_to_maturity)) %>%
      E()
  }

  if("vega" %in% greek) {
    result["vega"] <-
      (W_T^2/(volatility*time_to_maturity) - W_T - 1/volatility) %>%
      E()
  }

  if("rho" %in% greek) {
    result["rho"] <-
      (time_to_maturity * (W_T/(volatility*time_to_maturity) - 1)) %>%
      E()
  }

  if("theta" %in% greek) {
    result["theta"] <-
      -(W_T^2/(2*time_to_maturity^2) +
         (r - volatility^2/2)*W_T/(volatility*time_to_maturity) -
         (1/(2*time_to_maturity) + r)) %>%
      E()
  }

  if("gamma" %in% greek) {
    result["gamma"] <-
      ((1 / (initial_price^2 * volatility * time_to_maturity)) *
      (W_T^2/(volatility*time_to_maturity) - W_T - 1/volatility)) %>%
      E()
  }

  return(result)

}

