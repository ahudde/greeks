#' Computes the Greeks of an Asian option with the Malliavin Monte Carlo
#' Method in the Black Scholes model
#'
#' @export
#'
#' @import "stats"
#' @import "Rcpp"
#' @importFrom "matrixStats" "rowCumsums" "rowSums2"
#' @importFrom "dqrng" "dqrnorm" "dqset.seed"
#'
#' @param initial_price - initial price of the underlying asset.
#' @param exercise_price - strike price of the option.
#' @param r - risk-free interest rate.
#' @param time_to_maturity - time to maturity.
#' @param volatility - volatility of the underlying asset.
#' @param dividend_yield - dividend yield.
#' @param payoff - the payoff function, either a string in ("call", "put",
#' "digital_call", "digital_put"), or a function.
#' @param greek - the Greek to be calculated.
#' @param model - the model to be chosen in ("black_scholes", "jump_diffusion")
#' @param lambda - the lambda of the Poisson process in the jump-diffusion model
#' @param alpha - the alpha in the jump-diffusion model influences the jump size
#' @param jump_distribution - the distribution of the jumps, choose a function
#' which generates random numbers with the desired distribution
#' @param steps - the number of integration steps.
#' @param paths - the number of simulated paths.
#' @param seed - the seed of the random number generator
#' @param antithetic - if TRUE, antithetic random numbers will be chosen to
#' decrease variance
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
                                   model = "black_scholes",
                                   lambda = 0.2,
                                   alpha = 0.3,
                                   jump_distribution = function(n) stats::rt(n, df = 3),
                                   steps = round(time_to_maturity*252),
                                   paths = 10000,
                                   seed = 1,
                                   antithetic = FALSE) {

  dt <- time_to_maturity/steps

  result <- vector(mode = "numeric", length = length(greek))

  names(result) <- greek

  ## the payoff function ##

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

  ## the seed is set

  if(!is.na(seed)) {
    dqset.seed(seed)
  }

  ## the increments of the Brownian motion ###

  if(antithetic == FALSE) {
    W <-
     c(numeric(paths), dqrnorm(n = paths*steps, sd = sqrt(dt))) %>%
     matrix(nrow = paths, ncol = steps+1) %>%
     rowCumsums()
    # W      <- c(numeric(paths), dqrnorm(n = paths*steps, sd = sqrt(dt)))
    # dim(W) <- c(paths, steps+1)
    # W      <- rowCumsums(W)
  } else {
    W <- dqrnorm(n = paths/2*steps, sd = sqrt(dt)) %>%
      matrix(nrow = paths/2) %>%
      rowCumsums()
    W <- cbind(numeric(paths), rbind(W, -W))
  }

  W_T <- W[, steps + 1]

  X <- calc_X(W, dt, initial_price, volatility, r)

  if(model == "jump_diffusion") {

    Jumps <- c(numeric(paths), rpois(n = steps * paths, lambda = lambda*dt))

    for(i in which(Jumps != 0)) {
      Jumps[i] <- alpha * sum(jump_distribution(Jumps[i]))
    }

    Jumps <-
      Jumps %>%
      matrix(nrow = paths) %>%
      rowCumsums()

    X <- X * exp(Jumps)
  }# model == jump

  X_T <- X[, steps+1]

  ### the calculation of I_{(n)}, the integral \int_0^T t^n X_t dt ###

  I_0 <- (X[, 1]/2 + rowSums2(X, cols = 2:steps) + X[, steps+1]/2) * dt

  if("vega" %in% greek) {
    XW <- (X[, 1]*W[, 1]/2 + rowSums2(X[, 2:steps]*W[, 2:steps]) +
             X[, steps+1]*W[, steps + 1]/2) * dt
    tXW <- calc_tXW(X, W, steps, paths, dt)
  }

  if(length(intersect(greek, c("delta", "theta", "vega", "gamma")))) {
    I_1 <- calc_I_1(X, steps, dt)
    I_2 <- calc_I_2(X, steps, dt)
  }

  if("gamma" %in% greek) {
    I_3 <- calc_I_3(X, steps, dt)
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
      (r - 1/time_to_maturity +
         (1/(volatility * time_to_maturity) * I_0 * W_T -
            1/volatility * X_T * W_T + time_to_maturity * X_T) / I_1 +
         (1/time_to_maturity * I_0 * I_2 - I_2 * X_T) / (I_1^2)) %>%
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
      ((1/(volatility^2*initial_price^2)) *
         (2*volatility^2
          - 4*volatility*W_T*I_0/I_1
          + ((W_T^2 - time_to_maturity)*I_0 - 4*volatility^2*I_2)*I_0/I_1^2
          + volatility * (3*W_T*I_2 - volatility*I_3)*I_0^2/I_1^3
          + 3*volatility^2*I_0^2*I_2^2/I_1^4)) %>%
      E()
  }

  return(result)

}
