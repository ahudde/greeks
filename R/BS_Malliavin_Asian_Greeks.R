#' Computes the Greeks of an Asian option with the Malliavin Monte Carlo
#' Method in the Black Scholes model
#'
#' @export
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
#' @param greek - the Greek to be calculated
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
    greek = c("fair_value", "delta", "rho", "vega",
              "theta", "gamma"),
    steps = round(time_to_maturity*252),
    paths = 10000,
    seed = 1) {

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

  dt <- time_to_maturity/steps

  result <-
    matrix(ncol = length(greek),
           nrow = length(vectorized_param),
           dimnames = list(NULL, greek))

  ## the payoff function ##

  ## kommentieren?? was ist payoff_name??
  payoff_name <- payoff

  if (payoff == "call") {
    payoff <- function(x, exercise_price) {
      return(pmax(0, x - exercise_price))
    }
    dpayoff <- function(x, exercise_price) {
      return((x > exercise_price) + 0)
    }
  } else if (payoff == "put") {
    payoff <- function(x, exercise_price) {
      return(pmax(0, exercise_price - x))
    }
    dpayoff <- function(x, exercise_price) {
      return(-(x < exercise_price) + 0)
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

  # X <- calc_X(W, dt, volatility, r - dividend_yield)

  W_T <- W[, steps + 1]

  X_T <- X[, steps + 1]

  if (length(intersect(greek, c("vega", "vega_d")))) {
    XW <- calc_XW(X, W, steps, paths, dt)
    tXW <- calc_tXW(X, W, steps, paths, dt)
  }

  rm(W)

  ### the calculation of I_{(n)}, the integral (1/time_to_maturity) *
  ## (1/T) \int_0^T t^n X_t dt

  I_0 <- calc_I(X, steps, dt)

  if (length(intersect(
    greek,
    c("delta", "delta_d", "theta", "vega", "vega_d", "gamma", "gamma_kombi",
      "rho_d")))) {
    I_1 <- calc_I_1(X, steps, dt)
    I_2 <- calc_I_2(X, steps, dt)
  }

  if ("gamma" %in% greek) {
    I_3 <- calc_I_3(X, steps, dt)
  }

  for (i in 1:length(vectorized_param)) {

    assign(param, vectorized_param[i])

    I_0_geom <- initial_price * exp(calc_I(log_X, steps, dt) / time_to_maturity)

    #I_0_geom <-
    #  exp(calc_I(log(initial_price * X), steps, dt) / time_to_maturity)

    geom_asian_greeks_exact <-
      BS_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff_name,
        greek = greek
      )

    ## brauche ich E und dE noch??

    E <- function(weight) {
      return(exp(-(r - dividend_yield)*time_to_maturity) *
               mean(payoff(initial_price * I_0/time_to_maturity, exercise_price) * weight))
    }

    E_paths <- function(weight) {
      return(exp(-(r - dividend_yield) * time_to_maturity) *
               payoff(initial_price * I_0/time_to_maturity, exercise_price) * weight)
    }

    dE <- function(weight) {
      return(exp(-(r - dividend_yield) * time_to_maturity) *
               mean(dpayoff(initial_price * I_0/time_to_maturity, exercise_price) * weight))
    }

    dE_paths <- function(weight) {
      return(exp(-(r - dividend_yield) * time_to_maturity) *
               dpayoff(initial_price * I_0/time_to_maturity, exercise_price) * weight)
    }

    if ("fair_value" %in% greek) {

      fair_value <- E_paths(1)

      geom_fair_value <-
        exp(-(r - dividend_yield)*time_to_maturity) *
        payoff(I_0_geom, exercise_price)

      cont_fair_value <- geom_fair_value - geom_asian_greeks_exact["fair_value"]

      linear_model <- lm(fair_value ~ cont_fair_value)

      result[i, "fair_value"] <- linear_model$coefficients["(Intercept)"]

    }

    if ("delta" %in% greek) {

      delta_malliavin <-
        (1/(volatility * initial_price) *
           (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2))) %>%
        E_paths()

      delta_geom_malliavin <-
        2*exp(-r*time_to_maturity)/(initial_price*volatility*time_to_maturity) *
        (payoff(I_0_geom, exercise_price) * W_T)

      delta_d <- dE_paths(I_0 / time_to_maturity)

      cont_delta_malliavin <-
        delta_geom_malliavin - geom_asian_greeks_exact["delta"]

      delta_geom_d <- (1/initial_price) * exp(-(r - dividend_yield) * time_to_maturity) *
        dpayoff(I_0_geom, exercise_price) * I_0_geom

      cont_delta_d <-
        delta_geom_d - geom_asian_greeks_exact["delta"]

      linear_model <- lm(delta_d ~ cont_delta_malliavin + cont_delta_d)

      result[i, "delta"] <- linear_model$coefficients["(Intercept)"]

    }

    if ("delta_d" %in% greek) {
      result[i, "delta_d"] <- dE(I_0 / time_to_maturity)
    }

    if ("rho" %in% greek) {
      result[i, "rho"] <-
        (W_T/volatility - time_to_maturity) %>%
        E()
    }

    if ("rho_d" %in% greek) {
      result[i, "rho_d"] <-
        -time_to_maturity * E(1) + dE(initial_price * I_1/time_to_maturity)
    }

    if ("theta" %in% greek) {
      result[i, "theta"] <-
        ((r - dividend_yield) - 1/time_to_maturity +
           ((1/(volatility * time_to_maturity)) * I_0 * W_T -
              (1/volatility) * X_T * W_T + time_to_maturity * X_T) / I_1 +
           (1/time_to_maturity * I_0 * I_2 - I_2 * X_T) / (I_1^2)) %>%
        E()
    }

    if ("theta_d" %in% greek) {
      result[i, "theta_d"] <-
        (r - dividend_yield) * E(1) +
        dE(initial_price * (I_0/time_to_maturity^2 - X_T/time_to_maturity))
    }

    if ("vega" %in% greek) {
      result[i, "vega"] <-
        ((1 / volatility) *
           ( -(1 + volatility * W_T) +
               (W_T * XW - volatility * tXW) / I_1 +
               (volatility * XW * I_2) / I_1^2)) %>%
        E()
    }

    if ("vega_d" %in% greek) {
      result[i, "vega_d"] <-
        ((initial_price / time_to_maturity) * (XW - volatility * I_1)) %>%
        dE()
    }

    if ("gamma" %in% greek) {
      result[i, "gamma"] <-
        ((1/(volatility^2*initial_price^2)) *
           (2*volatility^2
            - 4*volatility*W_T*I_0/I_1
            + ((W_T^2 - time_to_maturity)*I_0 - 4*volatility^2*I_2)*I_0/I_1^2
            + volatility * (3*W_T*I_2 - volatility*I_3)*I_0^2/I_1^3
            + 3*volatility^2*I_0^2*I_2^2/I_1^4)) %>%
        E()
    }

    if ("gamma_kombi" %in% greek) {
      result[i, "gamma_kombi"] <-
        (1/(volatility * initial_price) *
           (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2))) %>%
        dE()
    }

  }

  return(drop(result))

}
