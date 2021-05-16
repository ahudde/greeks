#' This function calculates the fair value of an Asian call- or put-option by
#' with the Malliavin Monte Carlo Method in the Black Scholes model.
#' @export
#'
#' @importFrom "stats" "rnorm"
#'
#' @param initial_price - initial price of the underlying asset.
#' @param exercise_price - strike price of the option.
#' @param r - risk-free interest rate.
#' @param time_to_maturity - time to maturity.
#' @param volatility - volatility of the underlying asset.
#' @param dividend_yield - dividend yield.
#' @param payoff - the payoff function, either a string in ("put", "call"), or a
#' function.
#' @param greek - the greek to be calculated.
#' @param model - the model to be chosen.
#' @param steps - the number of integration steps.
#' @param paths - the number of simulated paths.
#' @param seed - the seed of the random number generator
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples Malliavin_Asian_Greeks(initial_price = 110, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "rho"), payoff = "put")
#'

Malliavin_Asian_Greeks <- function(initial_price = 100,
                                   exercise_price = 100,
                                   r = 0,
                                   time_to_maturity = 1,
                                   volatility = 0.3,
                                   dividend_yield = 0,
                                   payoff = "call",
                                   greek = c("fair_value", "delta", "rho",
                                             "theta"),
                                   model = "Black Scholes",
                                   steps = time_to_maturity*252,
                                   paths = 10000,
                                   seed = 1) {

  dt <- 1/steps

  ## the seed is just set locally
  old <- .Random.seed
  on.exit( { .Random.seed <<- old } )

  if(!is.na(seed)) {
    set.seed(seed)
  }

  result <- vector(mode = "numeric", length = length(greek))

  names(result) <- greek

  ## the increments of the Brownian motion ###

  W <- rnorm(n = paths*steps, sd = sqrt(dt)) %>%
    matrix(nrow=paths) %>%
    apply(MARGIN=1, FUN=cumsum) %>%
    t()

  W <- cbind(numeric(paths), W)

  W_T <- W[, steps + 1]

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
    }

  ### the calculation of I_{(n)}, the integral \int_0^T t^n X_t dt ###

  if(model == "Black Scholes") {

    X <- matrix(nrow = paths, ncol = steps+1)

    for(i in 1:(steps + 1)) {
      X[, i] <- initial_price *
        exp((r-(volatility^2)/2) * (i-1)*dt + (volatility*W[, i]))
    }
  } else {
    print("Unknown model")
    return()
  }

  X_T <- X[, steps + 1]

  I_0 <- dt * rowSums(X[, -1])

  I_1 <- dt * rowSums(seq(from = dt, to = time_to_maturity, by = dt) * X[, -1])

  I_2 <- dt * rowSums(seq(from = dt, to = time_to_maturity, by = dt)^2 * X[, -1])

  I_3 <- dt * rowSums(seq(from = dt, to = time_to_maturity, by = dt)^3 * X[, -1])

  XW <- dt * rowSums(X[, -1] * W[, -1])

  tXW <- dt * rowSums(seq(from = dt, to = time_to_maturity, by = dt) * X[, -1] * W[, -1])

  E <- function(weight) {
    return(exp(-r*time_to_maturity) * mean(payoff(I_0/time_to_maturity) * weight))
  }

  if("fair_value" %in% greek) {
    result["fair_value"] <-
      E(1)
  }

  if("delta" %in% greek) {
    result["delta"] <-
      1/(volatility * initial_price) *
      (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2)) %>%
      E()
  }

  if("rho" %in% greek) {
    result["rho"] <-
      (W_T/volatility - time_to_maturity) %>%
      E()
  }

  if("theta" %in% greek) {
    result["theta"] <-
      (r -
      1/T +
      (1/(volatility*time_to_maturity)*I_0*W_T - 1/volatility*X_T*W_T - T*X_T)/I_1 +
      (1/T*I_0*I_2 + I_2*W_T)/(I_1^2)) %>%
      E()
  }

  return(result)

}
