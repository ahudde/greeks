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

bool greek_matches(const CharacterVector& greek,
                   int i,
                   const std::string& target) {
  if (CharacterVector::is_na(greek[i])) {
    return false;
  }

  return as<std::string>(greek[i]) == target;
}

void assign_first_requested(NumericVector& result,
                            const CharacterVector& greek,
                            const std::string& target,
                            double value) {
  for (int i = 0; i < greek.size(); ++i) {
    if (greek_matches(greek, i, target)) {
      result[i] = value;
      return;
    }
  }
}

} // namespace

// [[Rcpp::export]]
NumericVector BS_Geometric_Asian_Greeks_cpp(double initial_price,
                                            double exercise_price,
                                            double r,
                                            double time_to_maturity,
                                            double volatility,
                                            double dividend_yield,
                                            std::string payoff,
                                            CharacterVector greek) {

  NumericVector result(greek.size(), NA_REAL);
  result.names() = greek;

  const double spot = initial_price;
  const double strike = exercise_price;
  const double time = time_to_maturity;
  const double sigma = volatility;
  const double rate_minus_dividend = r - dividend_yield;
  const double log_moneyness = std::log(spot / strike);
  const double sqrt_time = std::sqrt(time);
  const double sqrt_time_over_3 = std::sqrt(time / 3.0);
  const double sqrt_3 = std::sqrt(3.0);
  const double sqrt_3_time = std::sqrt(3.0 * time);

  const double d_geom =
    (log_moneyness +
     (time / 2.0) * (rate_minus_dividend - (sigma * sigma) / 2.0)) /
    (sigma * sqrt_time_over_3);

  const double d_geom_delta =
    sqrt_3 / (spot * sigma * sqrt_time);
  const double d_geom_vega =
    -sqrt_3_time *
    (0.25 +
     (log_moneyness / (sigma * sigma * time)) +
     (rate_minus_dividend / (2.0 * sigma * sigma)));
  const double d_geom_theta =
    -sqrt_3 * log_moneyness /
    (2.0 * sigma * std::pow(time, 1.5)) +
    sqrt_3 / (4.0 * sigma * sqrt_time) *
    (rate_minus_dividend - (sigma * sigma) / 2.0);
  const double d_geom_gamma =
    -sqrt_3 / (spot * spot * sigma * sqrt_time);

  const double normal_shift = sigma * sqrt_time_over_3;
  const double adjusted_d_geom = d_geom + normal_shift;
  const double discount_factor = std::exp(-rate_minus_dividend * time);
  const double geometric_prefactor =
    std::exp(-(time / 2.0) *
             (rate_minus_dividend + (sigma * sigma) / 6.0));
  const double discounted_strike = discount_factor * strike;

  const double normal_shift_vega = sqrt_time_over_3;
  const double d_geom_vega_second =
    2.0 * sqrt_3_time *
    (log_moneyness / time + rate_minus_dividend / 2.0) /
    (sigma * sigma * sigma);
  const double adjusted_d_geom_vega = d_geom_vega + normal_shift_vega;
  const double adjusted_d_geom_vega_second = d_geom_vega_second;
  const double prefactor_vega_ratio = -sigma * time / 6.0;
  const double prefactor_vega_ratio_second = -time / 6.0;

  if (payoff == "call") {
    const double call_fair_value =
      spot * geometric_prefactor * standard_normal_cdf(adjusted_d_geom) -
      discounted_strike * standard_normal_cdf(d_geom);

    const double call_delta =
      geometric_prefactor *
      (standard_normal_cdf(adjusted_d_geom) +
       spot * standard_normal_pdf(adjusted_d_geom) * d_geom_delta) -
      discounted_strike * standard_normal_pdf(d_geom) * d_geom_delta;

    const double call_vega =
      spot * geometric_prefactor *
      (prefactor_vega_ratio * standard_normal_cdf(adjusted_d_geom) +
       standard_normal_pdf(adjusted_d_geom) * adjusted_d_geom_vega) -
      discounted_strike * standard_normal_pdf(d_geom) * d_geom_vega;

    const double call_rho =
      spot * geometric_prefactor *
      ((-time / 2.0) * standard_normal_cdf(adjusted_d_geom) +
       standard_normal_pdf(adjusted_d_geom) *
       (sqrt_3_time / (2.0 * sigma))) +
      strike * discount_factor *
      (time * standard_normal_cdf(d_geom) -
       standard_normal_pdf(d_geom) * sqrt_3_time / (2.0 * sigma));

    const double call_theta =
      -spot * geometric_prefactor *
      (-0.5 * (rate_minus_dividend + (sigma * sigma) / 6.0) *
       standard_normal_cdf(adjusted_d_geom) +
       standard_normal_pdf(adjusted_d_geom) *
       (d_geom_theta + sigma / (2.0 * std::sqrt(3.0 * time)))) +
      strike * discount_factor *
      (-(rate_minus_dividend) * standard_normal_cdf(d_geom) +
       standard_normal_pdf(d_geom) * d_geom_theta);

    const double call_gamma =
      geometric_prefactor * standard_normal_pdf(adjusted_d_geom) *
      (2.0 * d_geom_delta -
       spot * adjusted_d_geom * d_geom_delta * d_geom_delta +
       spot * d_geom_gamma) +
      discounted_strike * standard_normal_pdf(d_geom) *
      (d_geom * d_geom_delta * d_geom_delta - d_geom_gamma);

    const double call_vomma =
      spot * geometric_prefactor *
      ((prefactor_vega_ratio * prefactor_vega_ratio +
        prefactor_vega_ratio_second) *
       standard_normal_cdf(adjusted_d_geom) +
       (2.0 * prefactor_vega_ratio * adjusted_d_geom_vega -
        adjusted_d_geom * adjusted_d_geom_vega * adjusted_d_geom_vega +
        adjusted_d_geom_vega_second) *
       standard_normal_pdf(adjusted_d_geom)) +
      discounted_strike * standard_normal_pdf(d_geom) *
      (d_geom * d_geom_vega * d_geom_vega - d_geom_vega_second);

    assign_first_requested(result, greek, "fair_value", call_fair_value);
    assign_first_requested(result, greek, "delta", call_delta);
    assign_first_requested(result, greek, "vega", call_vega);
    assign_first_requested(result, greek, "rho", call_rho);
    assign_first_requested(result, greek, "theta", call_theta);
    assign_first_requested(result, greek, "gamma", call_gamma);
    assign_first_requested(result, greek, "vomma", call_vomma);
  }

  if (payoff == "put") {
    const double put_argument = -adjusted_d_geom;
    const double minus_d_geom = -d_geom;

    const double put_fair_value =
      -spot * geometric_prefactor * standard_normal_cdf(put_argument) +
      discounted_strike * standard_normal_cdf(minus_d_geom);

    const double put_delta =
      -geometric_prefactor *
      (standard_normal_cdf(put_argument) -
       spot * standard_normal_pdf(put_argument) * d_geom_delta) -
      discounted_strike * standard_normal_pdf(minus_d_geom) * d_geom_delta;

    const double put_vega =
      -spot * geometric_prefactor *
      (prefactor_vega_ratio * standard_normal_cdf(put_argument) -
       standard_normal_pdf(put_argument) * adjusted_d_geom_vega) -
      discounted_strike * standard_normal_pdf(minus_d_geom) * d_geom_vega;

    const double put_rho =
      -spot * geometric_prefactor *
      ((-time / 2.0) * standard_normal_cdf(put_argument) -
       standard_normal_pdf(put_argument) *
       (sqrt_3_time / (2.0 * sigma))) -
      strike * discount_factor *
      (time * standard_normal_cdf(minus_d_geom) +
       standard_normal_pdf(minus_d_geom) * sqrt_3_time / (2.0 * sigma));

    const double put_theta =
      spot * geometric_prefactor *
      (-0.5 * (rate_minus_dividend + (sigma * sigma) / 6.0) *
       standard_normal_cdf(put_argument) -
       standard_normal_pdf(put_argument) *
       (d_geom_theta + sigma / (2.0 * std::sqrt(3.0 * time)))) +
      strike * discount_factor *
      (rate_minus_dividend * standard_normal_cdf(minus_d_geom) +
       standard_normal_pdf(minus_d_geom) * d_geom_theta);

    const double put_gamma =
      geometric_prefactor * standard_normal_pdf(put_argument) *
      (2.0 * d_geom_delta -
       spot * put_argument * d_geom_delta * d_geom_delta +
       spot * d_geom_gamma) -
      discounted_strike * standard_normal_pdf(minus_d_geom) *
      (d_geom * d_geom_delta * d_geom_delta - d_geom_gamma);

    const double put_vomma =
      -spot * geometric_prefactor *
      ((prefactor_vega_ratio * prefactor_vega_ratio +
        prefactor_vega_ratio_second) *
       standard_normal_cdf(put_argument) +
       (-2.0 * prefactor_vega_ratio * adjusted_d_geom_vega +
        adjusted_d_geom * adjusted_d_geom_vega * adjusted_d_geom_vega -
        adjusted_d_geom_vega_second) *
       standard_normal_pdf(adjusted_d_geom)) +
      discounted_strike * standard_normal_pdf(d_geom) *
      (d_geom * d_geom_vega * d_geom_vega - d_geom_vega_second);

    assign_first_requested(result, greek, "fair_value", put_fair_value);
    assign_first_requested(result, greek, "delta", put_delta);
    assign_first_requested(result, greek, "vega", put_vega);
    assign_first_requested(result, greek, "rho", put_rho);
    assign_first_requested(result, greek, "theta", put_theta);
    assign_first_requested(result, greek, "gamma", put_gamma);
    assign_first_requested(result, greek, "vomma", put_vomma);
  }

  return result;
}
