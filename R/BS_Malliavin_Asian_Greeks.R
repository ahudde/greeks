#' @title
#' Computes the Greeks of an Asian option with the Malliavin Monte Carlo Method
#' in the Black Scholes model
#'
#' @description
#' For a descriptoin see [Malliavin_Asian_Greeks]
#'
#' @export
#'
#' @seealso [Malliavin_Asian_Greeks] for a greater set of Greeks and also
#' in the jump diffusion model
#' @seealso [Greeks_UI] for an interactive visualization
#'
#' @import "stats"
#' @import "Rcpp"
#' @importFrom "dqrng" "dqrnorm" "dqset.seed"
#'
#' @param initial_price - initial price of the underlying asset, can also be a
#' vector
#' @param exercise_price - strike price of the option, can also be a vector
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param volatility - volatility of the underlying asset
#' @param dividend_yield - dividend yield
#' @param payoff - the payoff function, either a string in ("call", "put"), or a
#' function
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "rho",
#' "vega")
#' @param steps - the number of integration steps
#' @param paths - the number of simulated paths
#' @param seed - the seed of the random number generator
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples BS_Malliavin_Asian_Greeks(initial_price = 110, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "rho"), payoff = "put")
#'

BS_Malliavin_Asian_Greeks <- function(
    initial_price = 100,
    exercise_price = 100,
    r = 0,
    time_to_maturity = 1,
    volatility = 0.3,
    dividend_yield = 0,
    payoff = "call",
    greek = c("fair_value", "delta", "vega", "rho"),
    steps = round(time_to_maturity*252),
    paths = 1000,
    seed = 1) {

  ## cache stores value in order to compute them just once
  cache <- list()

  ## Finds the vectorized parameter, stores its name in `param` and its values
  ## in vectorized_param

  params <- c("initial_price", "exercise_price", "r", "time_to_maturity",
              "volatility", "dividend_yield")

  param <- params[1]
  vectorized_param <- get(param)

  for (p in params) {
    if (length(get(p)) >= 2) {
      vectorized_param <- get(p)
      param <- p
      break
    }
  }

  ## dt is the step size

  dt <- time_to_maturity/steps

  result <-
    matrix(ncol = length(greek),
           nrow = length(vectorized_param),
           dimnames = list(NULL, greek)) * NA

  ## the payoff function ##

  if (payoff == "call") {
    payoff_function <- function(x, exercise_price) {
      return(pmax(0, x - exercise_price))
    }
  } else if (payoff == "put") {
    payoff_function <- function(x, exercise_price) {
      return(pmax(0, exercise_price - x))
    }
  }

  ## the seed is set

  if (!is.na(seed)) {
    dqset.seed(seed)
  }

  gaussian_random_numbers <- dqrnorm(n = paths*steps, sd = sqrt(dt))
  W <- make_BM(gaussian_random_numbers, paths = paths, steps = steps)

  log_X <- calc_log_X(W, dt, volatility, r - dividend_yield)

  X <- exp(log_X)

  W_T <- W[, steps + 1]

  X_T <- X[, steps + 1]

  if ("vega" %in% greek) {
    XW <- calc_XW(X, W, steps, paths, dt)
    tXW <- calc_tXW(X, W, steps, paths, dt)
  }

  if ("vega" %in% greek) {
    I_W <- calc_I(W, steps, dt)
  }

  rm(W)

  ### the calculation of I_{(n)}, the integral (1/time_to_maturity) *
  ## (1/T) \int_0^T t^n X_t dt

  I_0 <- calc_I(X, steps, dt)

  if (length(intersect(
    greek,
    c("delta", "vega")))) {
    I_1 <- calc_I_1(X, steps, dt)
    I_2 <- calc_I_2(X, steps, dt)
  }

  cache$`exp(calc_I(log_X, steps, dt) / time_to_maturity)` <-
    exp(calc_I(log_X, steps, dt) / time_to_maturity)

  if("delta" %in% greek) {
    cache$`(1/(volatility) * (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2)))` <-
      (1/(volatility) *
         (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2)))
  }

  if ("vega" %in% greek) {
    cache$`vega_weight` <-
      ((1 / volatility) *
         ( -(1 + volatility * W_T) + (W_T * XW - volatility * tXW) / I_1 +
           (volatility * XW * I_2) / I_1^2))
  }

  for (i in 1:length(vectorized_param)) {

    assign(param, vectorized_param[i])

    I_0_geom <- initial_price * cache$`exp(calc_I(log_X, steps, dt) / time_to_maturity)`

    cache$`payoff_function(I_0_geom, exercise_price)` <-
      payoff_function(I_0_geom, exercise_price)

    geom_asian_greeks_exact <-
      BS_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff,
        greek = greek
      )

    E_paths <- function(weight) {
      return(
        exp(-(r - dividend_yield) * time_to_maturity) *
          payoff_function(initial_price * I_0/time_to_maturity, exercise_price) *
               weight)
    }

    if("fair_value" %in% greek || "rho" %in% greek) {

      geom_fair_value <-
        exp(-(r - dividend_yield)*time_to_maturity) *
        cache$`payoff_function(I_0_geom, exercise_price)`

    }

    if ("fair_value" %in% greek) {

      fair_value <- E_paths(1)

      cont_fair_value <- geom_fair_value - geom_asian_greeks_exact["fair_value"]

      linear_model <- lm(fair_value ~ cont_fair_value)

      result[i, "fair_value"] <- linear_model$coefficients["(Intercept)"]

    }

    if("delta" %in% greek || "rho" %in% greek) {

      delta_geom_malliavin <-
        2*exp(-r*time_to_maturity)/(initial_price*volatility*time_to_maturity) *
        (cache$`payoff_function(I_0_geom, exercise_price)` * W_T)

    }

    if ("delta" %in% greek) {

      delta_malliavin <-
        (cache$`(1/(volatility) * (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2)))` /
           initial_price) %>%
        E_paths()

      cont_delta_malliavin <-
        delta_geom_malliavin - geom_asian_greeks_exact["delta"]

      linear_model <- lm(delta_malliavin ~ cont_delta_malliavin)

      result[i, "delta"] <- linear_model$coefficients["(Intercept)"]

    }

    if ("rho" %in% greek) {
      rho_malliavin <-
        (W_T/volatility - time_to_maturity) %>%
        E_paths()

      rho_geom_malliavin <-
        -time_to_maturity * geom_fair_value +
        (initial_price * time_to_maturity)/2 * delta_geom_malliavin

      cont_rho_malliavin <-
        rho_geom_malliavin - geom_asian_greeks_exact["rho"]

      linear_model <- lm(rho_malliavin ~ cont_rho_malliavin)

      result[i, "rho"] <- linear_model$coefficients["(Intercept)"]

    }

    if ("vega" %in% greek) {

      vega_geom_malliavin <-
        exp(-r*time_to_maturity) *
        payoff_function(I_0_geom, exercise_price) *
        (2/(volatility * time_to_maturity**2) * W_T * I_W - 1/volatility - W_T)

      cont_vega_malliavin <-
        vega_geom_malliavin - geom_asian_greeks_exact["vega"]

      vega_malliavin <-
        cache$`vega_weight` %>%
        E_paths()

      linear_model <- lm(vega_malliavin ~ cont_vega_malliavin)

      result[i, "vega"] <- linear_model$coefficients["(Intercept)"]

    }

  }

  return(drop(result))

}
