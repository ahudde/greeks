#' @title
#' Computes the Greeks of a geometric Asian option with the Malliavin Monte
#' Carlo Method in the Black Scholes- or Jump diffusion model
#'
#' @description
#' In contrast to Asian options (see [Malliavin_Asian_Greeks]), geometric Asian
#' options evaluate the geometric average
#' \eqn{\exp \left( \frac{1}{T} \int_0^T \ln S_t dt \right)}, where
#' \eqn{S_t} is the price of the underlying asset at time \eqn{t} and \eqn{T} is
#' the time-to-maturity of the option (see
#'
#' [en.wikipedia.org/wiki/Asian_option#European_Asian_call_and_put_options_with_geometric_averaging](https://en.wikipedia.org/wiki/Asian_option#European_Asian_call_and_put_options_with_geometric_averaging)).
#' For more details on the definition of Greeks see [Greeks], and for a
#' description of the Malliavin Monte Carlo Method for Greeks see for example
#' (Hudde & Rüschendorf, 2023).
#'
#' @export
#'
#' @seealso [BS_Geometric_Asian_Greeks] for exact and fast computation in the
#' Black Scholes model and for put- and call payoff functions
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
#' @param payoff - the payoff function, either a string in ("call", "put",
#' "digital_call", "digital_put"), or a function
#' @param greek - the Greek to be calculated
#' @param model - the model to be chosen in ("black_scholes", "jump_diffusion")
#' @param lambda - the lambda of the Poisson process in the jump-diffusion model
#' @param alpha - the alpha in the jump-diffusion model influences the jump size
#' @param jump_distribution - the distribution of the jumps, choose a function
#' which generates random numbers with the desired distribution
#' @param steps - the number of integration steps
#' @param paths - the number of simulated paths
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
#' @references
#' Hudde, A., & Rüschendorf, L. (2023). European and Asian Greeks for Exponential Lévy Processes. Methodol Comput Appl Probab, 25 (39). \doi{10.1007/s11009-023-10014-5}
#'

Malliavin_Geometric_Asian_Greeks <- function(
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
    seed = 1,
    antithetic = FALSE) {

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
           dimnames = list(NULL, greek)) * NA

  # the payoff function
  if (inherits(payoff, "function")) {
    print("custom payoff")
  } else if (payoff == "call") {
    payoff <- function(x, exercise_price) {
      return(pmax(0, x - exercise_price))
    }
  } else if (payoff == "put") {
    payoff <- function(x, exercise_price) {
      return(pmax(0, exercise_price - x))
    }
  } else if (payoff == "digital_call") {
    payoff <- function(x, exercise_price) {ifelse(x >= exercise_price, 1, 0)
    }
  } else if (payoff == "digital_put") {
    payoff <- function(x, exercise_price) {ifelse(x <= exercise_price, 1, 0)
    }
  }

  # the seed is set
  if (!is.na(seed)) {
    dqset.seed(seed)
  }

  W <- make_BM(dqrnorm(n = paths*steps, sd = sqrt(dt)), paths = paths, steps = steps)

  X <- calc_X(W, dt, volatility, r - dividend_yield)

  if (model == "jump_diffusion") {

    Jumps <- c(numeric(paths), rpois(n = steps * paths, lambda = lambda *
                                       dt))
    for (i in which(Jumps != 0)) {
      Jumps[i] <- alpha * sum(jump_distribution(Jumps[i]))
    }
    Jumps <- Jumps %>% matrix(nrow = paths) %>% rowCumsums()
    X <- X * exp(Jumps)

  } # model == "jump_diffusion"

  W_T <- W[, steps + 1]

  X_T <- X[, steps + 1]

  # the calculation of I_W, the integral \int_0^T W_t dt
  I_W <- calc_I(W, steps, dt)

  # the calculation of I_{(n)}, the integral \int_0^T t^n X_t dt
  I_0 <- calc_I(X, steps, dt)

  # TODO: comment
  if (length(intersect(greek, c("delta", "theta", "vega", "gamma")))) {
    I_1 <- calc_I_1(X, steps, dt)
    I_2 <- calc_I_2(X, steps, dt)
  }

  # the calculation of I_ln_X, the integral \int_0^T ln(X_t) dt
  if("theta" %in% greek) {
    I_ln_X <- calc_I(log(X), steps, dt)
  }

  if ("gamma" %in% greek) {
    I_3 <- calc_I_3(X, steps, dt)
  }

  for (i in 1:length(vectorized_param)) {

    assign(param, vectorized_param[i])

    # the calculation of the geometric average of X
    I_0_geom <-
      exp(calc_I(log(initial_price * X), steps, dt) / time_to_maturity)

    # the value of the greek, given the Malliavin weight
    E_I_0_geom <- function(weight) {
      return(exp(-(r - dividend_yield) * time_to_maturity) *
               mean(payoff(I_0_geom, exercise_price) * weight))
    }

    if ("fair_value" %in% greek) {
      result[i, "fair_value"] <- E_I_0_geom(1)
    } #fair_value

    if ("delta" %in% greek) {
      result[i, "delta"] <-
        (2/(initial_price*volatility*time_to_maturity)) * E_I_0_geom(W_T)
    } #delta

    if ("rho" %in% greek) {
      result[i, "rho"] <-
        (W_T/volatility - time_to_maturity) %>%
        E_I_0_geom()
    } #rho

    if ("theta" %in% greek) {
      result[i, "theta"] <-
        ((r - dividend_yield) +
           (2 / (volatility * time_to_maturity^3)) * W_T * I_ln_X -
           (1 / time_to_maturity) -
           (2 / (volatility * time_to_maturity^2)) * log(X_T) * W_T +
           (2 / time_to_maturity)) %>%
        E_I_0_geom()
    } #theta

    if ("vega" %in% greek) {
      result[i, "vega"] <-
            (2/(volatility * time_to_maturity**2) * W_T * I_W -
                1/volatility - W_T)  %>%
                E_I_0_geom()

    } #vega

    if ("gamma" %in% greek) {
      result[i, "gamma"] <-
        ((1/(volatility^2*initial_price^2)) *
           (2*volatility^2
            - 4*volatility*W_T*I_0/I_1
            + ((W_T^2 - time_to_maturity)*I_0 - 4*volatility^2*I_2)*I_0/I_1^2
            + volatility * (3*W_T*I_2 - volatility*I_3)*I_0^2/I_1^3
            + 3*volatility^2*I_0^2*I_2^2/I_1^4)) %>%
        E_I_0_geom()
    } #gamma

  }

  return(drop(result))

}
