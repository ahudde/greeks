#' @title Computes the Greeks of various options in the Black Scholes model or
#' both in the Black Scholes model or a Jump Diffusion model in the case of Asian
#' Options, or in the Binomial options pricing model
#'
#' @description Greeks are derivatives of the option value with respect to
#' underlying parameters.
#' For instance, the Greek
#' \eqn{\Delta = \frac{\partial \text{fair\_value}}{\partial \text{initial\_price}}}
#' (Delta) measures how the price of an option changes with a minor change in
#' the underlying asset's price, while
#' \eqn{\Gamma = \frac{\partial \text{fair\_value}}{\partial \text{initial\_price}}}
#' (Gamma) measures how \eqn{\Delta} itself changes as the price of the
#' underlying asset shifts.
#' Greeks can be computed for different types of options:
#' For
#' - **European Greeks** see also [BS_European_Greeks] and [Malliavin_European_Greeks]
#' - **American Greeks** see also [Binomial_American_Greeks]
#' - **Asian Greeks** see also [BS_Malliavin_Asian_Greeks] and [Malliavin_Asian_Greeks]
#' - **Geometric Asian Greeks** see also [BS_Geometric_Asian_Greeks] and [Malliavin_Asian_Greeks]
#'
#' The Greeks are defined as the following partial derivatives of the option
#' value:
#' - \code{Delta} = \eqn{\Delta = \frac{\partial \text{fair\_value}}{\partial \text{initial\_price}}}, the derivative with respect to the price of the
#' underlying asset
#' - \code{Vega} = \eqn{\mathcal{V} = \frac{\partial \text{fair\_value}}{\partial \text{volatility}}},
#' the derivative with respect to the volatility
#' - \code{Theta} = \eqn{\Theta = -\frac{\partial \text{fair\_value}}{\partial \text{time\_to\_maturity}}},
#' the negative derivative with respect to the time until expiration of the
#' option
#' - \code{rho} = \eqn{\rho = \frac{\partial \text{fair\_value}}{\partial r}},
#' the derivative with respect to the risk-free interest rate
#' - \code{Epsilon} = \eqn{\epsilon = \frac{\partial \text{fair\_value}}{\partial \text{time\_to\_maturity}}},
#' the derivative with respect to the dividend yield of the underlying asset
#' - \code{Lambda} = \eqn{\lambda = \Delta \times \frac{\text{initial\_price}}{\text{exercise\_price}}}
#' - \code{Gamma} = \eqn{\Gamma = \frac{\partial^2 \text{fair\_value}}{\partial \text{initial\_price}^2}}, the second derivative with respect to the price of
#' the underlying asset
#' - \code{Vanna} = \eqn{\frac{\partial \Delta}{\partial \text{volatility}} = \frac{\partial^2 \text{fair\_value}}{\partial \text{intial\_price} \, \partial \text{volatility}}}, the derivative of \eqn{\Delta} with respect to the volatility
#' - \code{Vomma} = \eqn{\frac{\partial^2 \text{fair\_value}}{\partial \text{volatility}^2}}, the second derivative with respect to the volatility
#' - \code{Veta} = \eqn{
#' \frac{\partial \mathcal V}{\partial r}
#' = \frac{\partial^2 \text{fair\_value}}{\partial \text{volatility} \, \partial \text{time\_to\_maturity}}},
#' the derivative of \eqn{\mathcal V} with respect to the time until expiration
#' of the option
#' - \code{Vera} = \eqn{\frac{\partial^2 \text{fair\_value}}{\partial \text{volatiliy} \, \partial \text{r}}},
#' the derivative of \eqn{\mathcal V} with respect to the risk-free interest rate
#' - \code{Speed} = \eqn{\frac{\partial \Gamma}{\partial \text{initial\_price}} = \frac{\partial^3 \text{fair\_value}}{\partial \text{initial\_price}^3}},
#' the third derivative of the option value with respect to the price of the
#' underlying asset
#' - \code{Zomma} = \eqn{\frac{\Gamma}{\text{volatility}} = \frac{\partial^3 \text{fair\_value}}{\partial \text{volatility}^3}},
#' the derivative of Gamma with respect to the volatility
#'- \code{Color} = \eqn{\frac{\partial \Gamma}{\partial \text{r}} = \frac{\partial^3 \text{fair\_value}}{\partial \text{initial\_price}^2 \partial \text{r}}},
#' the derivative of Gamma with respect to the risk-free interest rate
#' - \code{Ultima} = \eqn{\frac{\partial \text{Vomma}}{\partial \text{volatility}} = \frac{\partial^3 \text{fair\_value}}{\partial \text{volatility}^3}},
#' the third derivative with respect to the volatility
#'
#' [Greeks] computes Greeks for the following option types:
#' - **European put- and call options**, which give to option holder the right
#' but not the obligation to sell (resp. buy) the underlying asset for a
#' specific price at a specific date.
#' If $K$ is the exercise price, and \eqn{S_T} the value of the underlying asset
#' at time-to-maturity \eqn{T}, a European options pay off the following amount
#' at expiration:
#'   - \eqn{\max\{K - S_T, 0\}} for a **put-option**
#'   - \eqn{\max\{S_T - K, 0\}} for a **call-option**
#' - **American put- and call options** are like European options, but allow
#' the holder to exercise at any time until expiration
#' - **European cash-or-nothing put- and call options** provide the holder with
#' a fixed amount of cash, if the value of the underlying asset is below (resp.
#' above) a certain strike price
#' - **European asset-or-nothing put- and call options** are similar to
#' cash-or-nothing options, but provide the holder with one share of the asset.
#' - **Asian put- and call options** have a similar payoff to European put- and
#' call options but differ from European options in that they are path dependent.
#' Not the price \eqn{S_T} of the underlying asset at time-to-maturity \eqn{T}
#' is evaluated, but the arithmetic average
#' \eqn{\frac{1}{T} \int_0^T S_t dt}.
#' We get the payoffs
#'   - \eqn{\max\{K - \frac{1}{T} \int_0^T S_t dt, 0\}} for an Asian
#'   **put-option**
#'   - \eqn{\max\{\frac{1}{T} \int_0^T S_t dt - K, 0\}} for an Asian
#'   **call-option**
#' - **Geometric Asian options** differ from Asian options in that the geometric
#' average
#' \eqn{\exp \left( \frac{1}{T} \int_0^T \ln S_t dt \right)} is evaluated.
#'
#' For reference see Hull (2022) or
#'
#' [en.wikipedia.org/wiki/Greeks_(finance)](https://en.wikipedia.org/wiki/Greeks_(finance)).
#'
#' @export
#'
#' @seealso [BS_European_Greeks] for option_type = "European"
#' @seealso [Binomial_American_Greeks] for option_type = "American"
#' @seealso [BS_Geometric_Asian_Greeks] for option_type = = "Geometric Asian"
#' and model = "black_scholes"
#' @seealso [BS_Malliavin_Asian_Greeks] for option_type = = "Asian"
#' and model = "black_scholes" and greek in c("fair_value", "delta", "rho",
#' "vega")
#' @seealso [Malliavin_Asian_Greeks] for more general cases of Asian Greeks
#' @seealso [Greeks_UI] for an interactive visualization
#'
#' @param initial_price - initial price of the underlying asset
#' @param exercise_price - strike price of the option
#' @param r - risk-free interest rate
#' @param time_to_maturity - time to maturity in years
#' @param dividend_yield - dividend yield
#' @param volatility - volatility of the underlying asset
#' @param model - the model to be chosen in ("black_scholes", "jump_diffusion")
#' @param option_type in c("European", "American", "Asian", "Geometric Asian",
#' "Digital", "Binomial) - the type of option to be considered
#' @param payoff - in c("call", "put", "cash_or_nothing_call",
#' "cash_or_nothing_put", "asset_or_nothing_call", "asset_or_nothing_put")
#' @param greek - Greeks to be calculated in c("fair_value", "delta", "vega",
#' "theta", "rho", "epsilon", "lambda", "gamma", "vanna", "charm", "vomma",
#' "veta", "vera", "speed", "zomma", "color", "ultima")
#' @param ... - ...	Other arguments passed on to methods
#' @param antithetic - if TRUE, antithetic random numbers will be chosen to
#' decrease variance
#'
#' @return Named vector containing the values of the Greeks specified in the
#' parameter \code{greek}.
#'
#' @examples
#' Greeks(initial_price = 100, exercise_price = 120, r = 0.01,
#' time_to_maturity = 5, volatility = 0.30, payoff = "call")
#'
#' Greeks(initial_price = 100, exercise_price = 100, r = -0.005,
#' time_to_maturity = 1, volatility = 0.30, payoff = "put",
#' option_type = "American")
#'
#' @references
#' Hull, J. C. (2022). Options, futures, and other derivatives (11th Edition). Pearson
#'
#' [en.wikipedia.org/wiki/Greeks_(finance)](https://en.wikipedia.org/wiki/Greeks_(finance))

Greeks <-
  function(initial_price,
           exercise_price,
           r,
           time_to_maturity,
           volatility,
           dividend_yield = 0,
           model = "Black_Scholes",
           option_type = "European",
           payoff = "call",
           greek = c("fair_value", "delta", "vega", "theta", "rho", "gamma"),
           antithetic = TRUE,
           ...){

    if (tolower(option_type) %in% c("digital", "european") &&
        tolower(model) == "black_scholes") {
      if (tolower(option_type) == "digital" &&
          tolower(payoff) %in% c("call", "put")) {
        stop("Please choose one of the following:
                'cash_or_nothing_call',
                'cash_or_nothing_put',
                'asset_or_nothing_call',
                'asset_or_nothing_put'.")
      }
      return(BS_European_Greeks(payoff = payoff,
                                greek = greek,
                                initial_price = initial_price,
                                exercise_price = exercise_price,
                                r = r,
                                time_to_maturity = time_to_maturity,
                                volatility = volatility,
                                dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "american" && tolower(model) == "black_scholes") {
      return(Binomial_American_Greeks(payoff = payoff,
                                      greek = greek,
                                      initial_price = initial_price,
                                      exercise_price = exercise_price,
                                      r = r,
                                      time_to_maturity = time_to_maturity,
                                      volatility = volatility,
                                      dividend_yield = dividend_yield,
                                      ...))
    }

    else if (tolower(option_type) == "geometric asian" && tolower(model) == "black_scholes") {
      return(BS_Geometric_Asian_Greeks(payoff = payoff,
                                       greek = greek,
                                       initial_price = initial_price,
                                       exercise_price = exercise_price,
                                       r = r,
                                       time_to_maturity = time_to_maturity,
                                       volatility = volatility,
                                       dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "asian" && tolower(model) == "black_scholes" &&
             all(greek %in% c("fair_value", "delta", "rho", "vega"))) {
      return(BS_Malliavin_Asian_Greeks(payoff = payoff,
                                       greek = greek,
                                       initial_price = initial_price,
                                       exercise_price = exercise_price,
                                       r = r,
                                       time_to_maturity = time_to_maturity,
                                       volatility = volatility,
                                       dividend_yield = dividend_yield))
    }

    else if (tolower(option_type) == "asian") {
      return(Malliavin_Asian_Greeks(payoff = payoff,
                                    greek = greek,
                                    initial_price = initial_price,
                                    exercise_price = exercise_price,
                                    r = r,
                                    time_to_maturity = time_to_maturity,
                                    volatility = volatility,
                                    dividend_yield = dividend_yield,
                                    model = model,
                                    antithetic = antithetic))
    }

    else {
      stop("Wrong input or not yet implemented.")
    }

  }
