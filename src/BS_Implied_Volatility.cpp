#include <Rcpp.h>
#include <cmath>
#include <string>

using namespace Rcpp;

namespace {

double standard_normal_cdf(double x) {
  return R::pnorm(x, 0.0, 1.0, 1, 0);
}

double standard_normal_pdf(double x) {
  return R::dnorm(x, 0.0, 1.0, 0);
}

bool is_supported_payoff(const std::string& payoff) {
  return payoff == "call" || payoff == "put";
}

void require_positive_finite(const char* name, double value) {
  if (!std::isfinite(value) || value <= 0.0) {
    stop("%s must be a positive finite number.", name);
  }
}

void require_finite(const char* name, double value) {
  if (!std::isfinite(value)) {
    stop("%s must be a finite number.", name);
  }
}

struct BlackScholesValues {
  double fair_value;
  double vega;
  double vomma;
};

BlackScholesValues black_scholes_values(double spot,
                                        double strike,
                                        double r,
                                        double time,
                                        double dividend_yield,
                                        double volatility,
                                        const std::string& payoff) {
  const double sqrt_time = std::sqrt(time);
  const double d1 =
    (std::log(spot / strike) +
     (r - dividend_yield + (volatility * volatility) / 2.0) * time) /
    (volatility * sqrt_time);
  const double d2 = d1 - volatility * sqrt_time;

  const double discounted_spot = spot * std::exp(-dividend_yield * time);
  const double discounted_strike = strike * std::exp(-r * time);

  double fair_value;
  if (payoff == "call") {
    fair_value =
      discounted_spot * standard_normal_cdf(d1) -
      discounted_strike * standard_normal_cdf(d2);
  } else {
    fair_value =
      discounted_strike * standard_normal_cdf(-d2) -
      discounted_spot * standard_normal_cdf(-d1);
  }

  const double vega = discounted_spot * standard_normal_pdf(d1) * sqrt_time;
  const double vomma = vega * d1 * d2 / volatility;

  return {fair_value, vega, vomma};
}

} // namespace

// [[Rcpp::export]]
double BS_Implied_Volatility_cpp(double option_price,
                                 double initial_price,
                                 double exercise_price,
                                 double r,
                                 double time_to_maturity,
                                 double dividend_yield,
                                 std::string payoff,
                                 double start_volatility,
                                 double precision,
                                 int max_iter) {
  require_finite("option_price", option_price);
  require_positive_finite("initial_price", initial_price);
  require_positive_finite("exercise_price", exercise_price);
  require_finite("r", r);
  require_positive_finite("time_to_maturity", time_to_maturity);
  require_finite("dividend_yield", dividend_yield);
  require_positive_finite("start_volatility", start_volatility);
  require_positive_finite("precision", precision);

  if (max_iter <= 0) {
    stop("max_iter must be a positive integer.");
  }

  if (!is_supported_payoff(payoff)) {
    stop("payoff must be one of 'call' or 'put'.");
  }

  const double option_price_zero_vol =
    black_scholes_values(initial_price, exercise_price, r, time_to_maturity,
                         dividend_yield, 1e-12, payoff).fair_value;

  if (option_price <= option_price_zero_vol) {
    stop("Option price is too low. Implied volatility is not defined.");
  }

  double volatility = start_volatility;
  BlackScholesValues values =
    black_scholes_values(initial_price, exercise_price, r, time_to_maturity,
                         dividend_yield, volatility, payoff);

  for (int iteration = 0; iteration < max_iter; ++iteration) {
    checkUserInterrupt();

    const double price_error = values.fair_value - option_price;

    if (std::abs(price_error) < precision) {
      return volatility;
    }

    volatility -=
      (2.0 * price_error * values.vega) /
      (2.0 * values.vega * values.vega - price_error * values.vomma);

    values =
      black_scholes_values(initial_price, exercise_price, r, time_to_maturity,
                           dividend_yield, volatility, payoff);
  }

  stop("Maximum number of iterations reached (max_iter = %d).", max_iter);
}
