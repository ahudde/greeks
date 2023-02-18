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
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param volatility - volatility of the underlying asset
#' @param dividend_yield - dividend yield
#' @param payoff - the payoff function, either a string in ("call", "put",
#' "digital_call", "digital_put"), or a function
#' @param greek - the Greek to be calculated
#' @param steps - the number of integration steps
#' @param paths - the number of simulated paths
#' @param seed - the seed of the random number generator
#' @param antithetic - if TRUE, antithetic random numbers will be chosen to
#' decrease variance
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples Asian_Greeks(initial_price = 110, exercise_price = 100,
#' r = 0.02, time_to_maturity = 4.5, dividend_yield = 0.015, volatility = 0.22,
#' greek = c("fair_value", "delta", "rho"), payoff = "put")
#'

BS_Asian_Greeks <- function(
    initial_price = 100,
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
    seed = 1) {

  params <- c("initial_price", "exercise_price", "r", "time_to_maturity",
              "volatility", "dividend_yield")

  param <- params[1]
  vectorized_param <- get(param)

  for (p in params) {
    if ( length(get(p)) >= 2) {
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

  ## kommentieren??
  payoff_name <- payoff

  if (inherits(payoff, "function")) {
    print("custom payoff")
  } else if (payoff == "call") {
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
  } else if (payoff == "digital_call") {
    payoff <- function(x, exercise_price) {ifelse(x >= exercise_price, 1, 0)
    }
  } else if (payoff == "digital_put") {
    payoff <- function(x, exercise_price) {ifelse(x <= exercise_price, 1, 0)
    }
  }

  ## the seed is set

  if (!is.na(seed)) {
    dqset.seed(seed)
  }

  W <- make_BM(dqrnorm(n = paths*steps, sd = sqrt(dt)), paths = paths, steps = steps)

  X <- calc_X(W, dt, volatility, r - dividend_yield)

  W_T <- W[, steps + 1]

  X_T <- X[, steps + 1]

  XW <- calc_XW(X, W, steps, paths, dt)
  tXW <- calc_tXW(X, W, steps, paths, dt)

  #rm(W) ??
  # I_W = int_0^T W_t dt
  I_W <- calc_I(W, steps, dt)

  ### the calculation of I_{(n)}, the integral \int_0^T t^n X_t dt ###

  I_0 <- calc_I(X, steps, dt)
  I_1 <- calc_I_1(X, steps, dt)
  I_2 <- calc_I_2(X, steps, dt)
  I_3 <- calc_I_3(X, steps, dt)

  for (i in 1:length(vectorized_param)) {

    geom_asian_greeks_exact <-
      BS_Geometric_Asian_Greeks(
        initial_price = initial_price,
        exercise_price = exercise_price,
        r = r,
        time_to_maturity = time_to_maturity,
        volatility = volatility,
        dividend_yield = dividend_yield,
        payoff = payoff_name,
        greek = c("fair_value", "delta", "vega", "rho", "theta", "gamma")
      )

    assign(param, vectorized_param[i])

    E <- function(weight) {
      return(exp(-(r - dividend_yield) * time_to_maturity) *
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

    # hier was erklären ??
    I_0_geom <-
      exp(calc_I(log(initial_price * X), steps, dt) / time_to_maturity)

    fair_value <-
      exp(-(r - dividend_yield)*time_to_maturity) *
      payoff(initial_price * I_0/time_to_maturity, exercise_price)

    geom_fair_value <-
      exp(-(r - dividend_yield)*time_to_maturity) *
      payoff(I_0_geom, exercise_price)

    # The Malliavin vectors

    delta_malliavin <-
      (1/(volatility * initial_price) *
         (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2))) %>%
      E_paths()

    vega_malliavin <-
      ((1 / volatility) *
         ( -(1 + volatility * W_T) +
             (W_T * XW - volatility * tXW) / I_1 +
             (volatility * XW * I_2) / I_1^2)) %>%
      E_paths()

    rho_malliavin <-
      (W_T/volatility - time_to_maturity) %>%
      E_paths()

    theta_malliavin <-
      ((r - dividend_yield) - 1/time_to_maturity +
         ((1/(volatility * time_to_maturity)) * I_0 * W_T -
            (1/volatility) * X_T * W_T + time_to_maturity * X_T) / I_1 +
         (1/time_to_maturity * I_0 * I_2 - I_2 * X_T) / (I_1^2)) %>%
      E_paths()

    gamma_malliavin <-
      ((1/(volatility^2*initial_price^2)) *
         (2*volatility^2
          - 4*volatility*W_T*I_0/I_1
          + ((W_T^2 - time_to_maturity)*I_0 - 4*volatility^2*I_2)*I_0/I_1^2
          + volatility * (3*W_T*I_2 - volatility*I_3)*I_0^2/I_1^3
          + 3*volatility^2*I_0^2*I_2^2/I_1^4)) %>%
      E_paths()

    # the geometric Malliavin vector

    delta_geom_malliavin <-
      2*exp(-r*time_to_maturity)/(initial_price*volatility*time_to_maturity) *
      (payoff(I_0_geom, exercise_price) * W_T)

    vega_geom_malliavin <-
      exp(-r*time_to_maturity) *
      payoff(I_0_geom, exercise_price) *
      (2/(volatility * time_to_maturity**2) * W_T * I_W -
         1/volatility -
         W_T)

    rho_geom_malliavin <-
      -time_to_maturity * geom_fair_value +
      (initial_price * time_to_maturity)/2 * delta_geom_malliavin

    # The Fd vectors

    delta_d <- dE_paths(I_0 / time_to_maturity)

    vega_d <-
      ((initial_price / time_to_maturity) * (XW - volatility * I_1)) %>%
      dE_paths()

    rho_fd <-
      -time_to_maturity *  E_paths(1) + dE_paths(initial_price * I_1/time_to_maturity)

    theta_d <-
      (r - dividend_yield) * E_paths(1) +
      dE_paths(initial_price * (I_0/time_to_maturity^2 - X_T/time_to_maturity))

    gamma_kombi <-
      (1/(volatility * initial_price) *
         (-volatility + I_0/I_1*W_T + volatility*I_0*I_2/(I_1^2))) %>%
      dE_paths()

    # the geometric fd vectors

    delta_geom_fd <- (1/initial_price) * exp(-(r - dividend_yield) * time_to_maturity) *
      dpayoff(I_0_geom, exercise_price) * I_0_geom

    vega_geom_fd <- exp(-(r - dividend_yield) * time_to_maturity) *
      dpayoff(I_0_geom, exercise_price) * I_0_geom *
      (I_W/time_to_maturity - volatility/2 * time_to_maturity)

    rho_geom_fd <-
      -time_to_maturity * exp(-r*time_to_maturity) * payoff(I_0_geom, exercise_price) +
      (time_to_maturity/2) * exp(-(r - dividend_yield) * time_to_maturity) *
      dpayoff(I_0_geom, exercise_price) * I_0_geom

    theta_geom_fd <-
      r * exp(-r*time_to_maturity) * payoff(I_0_geom, exercise_price) -
      exp(-r*time_to_maturity) *
      dpayoff(I_0_geom, exercise_price) * I_0_geom *
      (0.5 * (r - (volatility**2)/2) - (volatility/time_to_maturity**2) * I_W +
         (volatility/time_to_maturity) * W_T)

    # the Malliavin control variates

    cont_fair_value <- geom_fair_value - geom_asian_greeks_exact["fair_value"]

    cont_delta_malliavin <-
      delta_geom_malliavin - geom_asian_greeks_exact["delta"]

    cont_vega_malliavin <-
      vega_geom_malliavin - geom_asian_greeks_exact["vega"]

    cont_rho_malliavin <-
      rho_geom_malliavin - geom_asian_greeks_exact["rho"]

    # the FD control variates

    cont_delta_d <-
      delta_d - delta_malliavin

    cont_vega_d <-
      vega_d - vega_malliavin

    cont_rho_fd <-
      rho_fd - rho_malliavin

    cont_theta_d <-
      theta_d - theta_malliavin

    cont_gamma_d <-
      gamma_kombi - gamma_malliavin

    # the hoermann cv

    cont_hoermann <-
      (initial_price * I_0 - exercise_price) * (I_0_geom >= exercise_price)

    data <-
      tibble(
        cont_fair_value,
        cont_delta_malliavin,
        cont_vega_malliavin,
        cont_rho_malliavin,
        cont_delta_d,
        cont_vega_d,
        cont_rho_fd,
        cont_theta_d)

    if ("fair_value" %in% greek) {

      model <- lm(fair_value ~ ., data = data)

      result[i, "fair_value"] <- model$coefficients["(Intercept)"]

      print("fair_value")
      print(1 / (1 - summary(model)$r.squared))

    }


    if ("delta" %in% greek) {

      model <- lm(delta_malliavin ~ ., data = data)

      result[i, "delta"] <- model$coefficients["(Intercept)"]

      print("delta")
      print(1 / (1 - summary(model)$r.squared))

    }

    if ("rho" %in% greek) {

      model <- lm(rho_malliavin ~ ., data = data)

      result[i, "rho"] <- model$coefficients["(Intercept)"]

      print("rho")
      print(1 / (1 - summary(model)$r.squared))

    }

    if ("theta" %in% greek) {

      model <- lm(theta_malliavin ~ cont_theta_d)

      result[i, "theta"] <- model$coefficients["(Intercept)"]

      print("theta")
      print(1 / (1 - summary(model)$r.squared))

    }

    if ("vega" %in% greek) {

      model <- lm(vega_malliavin ~ cont_vega_malliavin)

      result[i, "vega"] <- model$coefficients["(Intercept)"]

      print("vega")
      print(1 / (1 - summary(model)$r.squared))

    }

    if ("gamma" %in% greek) {

      model <- lm(gamma_malliavin ~ ., data = data)

      result[i, "gamma"] <- model$coefficients["(Intercept)"]

      print("gamma")
      print(1 / (1 - summary(model)$r.squared))
    }

  }

  return(drop(result))

}
