#' This function calculates the fair value of an Asian option with the Malliavin
#' Monte Carlo Method in the Black Scholes model.
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
#' @param numeric_method - the numbers choosen for the numeric intergration,
#' either a string in ("monte_carlo", "monte_carlo_antithetic", "equal_distance)
#' or custum numbers in form of a matrix with nrows = paths
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
                                   greek = c("fair_value", "delta", "rho", "vega",
                                             "theta", "gamma"),
                                   model = "Black Scholes",
                                   steps = round(time_to_maturity*252),
                                   paths = 10000,
                                   seed = 1,
                                   numeric_method = "monte_carlo") {

  dt <- 1/steps

  result <- vector(mode = "numeric", length = length(greek))

  names(result) <- greek

  ## the seed is set locally

  if(!is.na(seed)) {
    old.seed <- .Random.seed
    set.seed(seed)
  }

  ## the increments of the Brownian motion ###

  if(numeric_method == "monte_carlo") {
    W <- rnorm(n = paths*steps, sd = sqrt(dt)) %>%
      matrix(nrow = paths) %>%
      apply(MARGIN = 1, FUN = cumsum) %>%
      t()
  } else if(numeric_method == "monte_carlo_antithetic") {
    W <- rnorm(n = paths/2*steps, sd = sqrt(dt)) %>%
      matrix(nrow = paths/2) %>%
      apply(MARGIN = 1, FUN = cumsum) %>%
      t()
    W <- rbind(W, -W)
  } else if(class(numeric_method) == "matrix" &&
                   prod(dim(numeric_method) == c(steps, paths))) {
    W <- numeric_method
  }

  if(!is.na(seed)) {
    .Random.seed <<- old.seed
  }

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

  X_T <- X[, steps+1]

  ### the calculation of I_{(n)}, the integral \int_0^T t^n X_t dt ###

  I_0 <- (X[, 1]/2 + rowSums(X[, 2:steps]) + X[, steps+1]/2) * dt

  XW <- (X[, 1]*W[, 1]/2 + rowSums(X[, 2:steps]*W[, 2:steps]) +
           X[, steps+1]*W[, steps + 1]/2) * dt

  tXW <- X[, steps+1]*W[, steps+1]/2 * (steps*dt) * dt

  I_1 <- X[, steps+1]/2 * (steps*dt) * dt
  I_2 <- X[, steps+1]/2 * (steps*dt)^2 * dt
  I_3 <- X[, steps+1]/2 * (steps*dt)^3 * dt

  for(i in 1:(steps-1)) {
    I_1 <- I_1 + X[, i+1] * (i*dt) * dt
    I_2 <- I_2 + X[, i+1] * (i*dt)^2 * dt
    I_3 <- I_3 + X[, i+1] * (i*dt)^3 * dt
    tXW <- tXW + X[, i+1]*W[, i+1] * (i*dt) * dt
  }

  E <- function(weight) {
    return(exp(-r*time_to_maturity) * mean(payoff(I_0/time_to_maturity) * weight))
  }

  if("fair_value" %in% greek) {
    result["fair_value"] <-
      E(1)
  }

  if("delta" %in% greek) {
    result["delta"] <-
      (1/(volatility * initial_price) *
      (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2))) %>%
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

  if("vega" %in% greek) {
    result["vega"] <-
    ((1 / volatility) *
      ( -(1 + volatility * W_T) +
          (W_T * XW - volatility * tXW) / I_1 +
          (volatility * XW * I_2) / I_1^2)) %>%
      E()
  }

  if("gamma" %in% greek) {
    result["gamma"] <-
      ((1/(volatility^2 * initial_price^2) *
      (((1+x) * volatility^2) -
      (((1+x)*W_T + 1) * volatility * I_0/I_1) +
      ((W_T^2*I_0 - (2*volatility + (1+x)*volatility^2)*I_2) * (I_0/I_1^2)) +
      ((W_T*I_2 + volatility*(2*I_2 - I_3)) * (I_0^2/I_1^3)) +
      ((3*volatility * I_0^2*I_2^2) / I_1^4)))) %>%
      E()
  }

  return(result)

}

