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

bool is_binary_payoff(const std::string& payoff) {
  return payoff == "cash_or_nothing_call" ||
    payoff == "cash_or_nothing_put" ||
    payoff == "asset_or_nothing_call" ||
    payoff == "asset_or_nothing_put";
}

bool is_valid_payoff(const std::string& payoff) {
  return payoff == "call" || payoff == "put" || is_binary_payoff(payoff);
}

std::string greek_name_at(const CharacterVector& greek, int i) {
  if (CharacterVector::is_na(greek[i])) {
    return "";
  }

  return as<std::string>(greek[i]);
}

} // namespace

// [[Rcpp::export]]
NumericVector BS_European_Greeks_cpp(double initial_price,
                                     double exercise_price,
                                     double r,
                                     double time_to_maturity,
                                     double volatility,
                                     double dividend_yield,
                                     std::string payoff,
                                     CharacterVector greek) {

  if (!is_valid_payoff(payoff)) {
    stop("payoff must be one of 'call', 'put', 'cash_or_nothing_call', "
         "'cash_or_nothing_put', 'asset_or_nothing_call', "
         "'asset_or_nothing_put'.");
  }

  const int n_greeks = greek.size();
  NumericVector result(n_greeks, NA_REAL);
  result.names() = greek;

  const double spot = initial_price;
  const double sigma = volatility;
  const double time = time_to_maturity;
  const double sqrt_time_to_maturity = std::sqrt(time);

  const double d1 =
    (std::log(spot / exercise_price) +
     (r - dividend_yield + (sigma * sigma) / 2.0) * time) /
    (sigma * sqrt_time_to_maturity);

  const double d2 = d1 - sigma * sqrt_time_to_maturity;

  const double exp_minus_dividend_yield_times_time_to_maturity =
    std::exp(-dividend_yield * time);
  const double exp_minus_r_times_time_to_maturity = std::exp(-r * time);

  const double pnorm_d1 = standard_normal_cdf(d1);
  const double dnorm_d1 = standard_normal_pdf(d1);
  const double pnorm_d2 = standard_normal_cdf(d2);

  if (payoff == "call") {
    for (int i = 0; i < n_greeks; ++i) {
      const std::string greek_name = greek_name_at(greek, i);

      if (greek_name == "fair_value") {
        result[i] =
          spot * exp_minus_dividend_yield_times_time_to_maturity * pnorm_d1 -
          exp_minus_r_times_time_to_maturity * exercise_price * pnorm_d2;
      } else if (greek_name == "delta") {
        result[i] =
          exp_minus_dividend_yield_times_time_to_maturity * pnorm_d1;
      } else if (greek_name == "theta") {
        result[i] =
          -spot * dnorm_d1 * sigma *
          exp_minus_dividend_yield_times_time_to_maturity /
          (2.0 * sqrt_time_to_maturity) +
          dividend_yield * spot * pnorm_d1 *
          exp_minus_dividend_yield_times_time_to_maturity -
          r * exercise_price * exp_minus_r_times_time_to_maturity * pnorm_d2;
      } else if (greek_name == "rho") {
        result[i] =
          exercise_price * time * exp_minus_r_times_time_to_maturity *
          pnorm_d2;
      } else if (greek_name == "epsilon") {
        result[i] =
          -spot * time * exp_minus_dividend_yield_times_time_to_maturity *
          pnorm_d1;
      } else if (greek_name == "lambda") {
        result[i] =
          spot *
          (exp_minus_dividend_yield_times_time_to_maturity * pnorm_d1) /
          (spot * exp_minus_dividend_yield_times_time_to_maturity * pnorm_d1 -
           exp_minus_r_times_time_to_maturity * exercise_price * pnorm_d2);
      } else if (greek_name == "charm") {
        result[i] =
          (dividend_yield * exp_minus_dividend_yield_times_time_to_maturity *
           pnorm_d1) -
          (exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
           (2.0 * (r - dividend_yield) * time -
            d2 * sigma * sqrt_time_to_maturity) /
           (2.0 * time * sigma * sqrt_time_to_maturity));
      }
    }
  }

  if (payoff == "put") {
    const double pnorm_minus_d2 = standard_normal_cdf(-d2);
    const double pnorm_minus_d1 = standard_normal_cdf(-d1);

    for (int i = 0; i < n_greeks; ++i) {
      const std::string greek_name = greek_name_at(greek, i);

      if (greek_name == "fair_value") {
        result[i] =
          exp_minus_r_times_time_to_maturity * exercise_price *
          pnorm_minus_d2 -
          spot * exp_minus_dividend_yield_times_time_to_maturity *
          pnorm_minus_d1;
      } else if (greek_name == "delta") {
        result[i] =
          -exp_minus_dividend_yield_times_time_to_maturity * pnorm_minus_d1;
      } else if (greek_name == "theta") {
        result[i] =
          -spot * dnorm_d1 * sigma *
          exp_minus_dividend_yield_times_time_to_maturity /
          (2.0 * sqrt_time_to_maturity) -
          dividend_yield * spot * pnorm_minus_d1 *
          exp_minus_dividend_yield_times_time_to_maturity +
          r * exercise_price * exp_minus_r_times_time_to_maturity *
          pnorm_minus_d2;
      } else if (greek_name == "rho") {
        result[i] =
          -exercise_price * time * exp_minus_r_times_time_to_maturity *
          pnorm_minus_d2;
      } else if (greek_name == "epsilon") {
        result[i] =
          spot * time * exp_minus_dividend_yield_times_time_to_maturity *
          pnorm_minus_d1;
      } else if (greek_name == "lambda") {
        result[i] =
          -spot *
          (exp_minus_dividend_yield_times_time_to_maturity * pnorm_minus_d1) /
          (exp_minus_r_times_time_to_maturity * exercise_price *
           pnorm_minus_d2 -
           spot * exp_minus_dividend_yield_times_time_to_maturity *
           pnorm_minus_d1);
      } else if (greek_name == "charm") {
        result[i] =
          (-dividend_yield * exp_minus_dividend_yield_times_time_to_maturity *
           pnorm_minus_d1) -
          (exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
           (2.0 * (r - dividend_yield) * time -
            d2 * sigma * sqrt_time_to_maturity) /
           (2.0 * time * sigma * sqrt_time_to_maturity));
      }
    }
  }

  if (payoff == "call" || payoff == "put") {
    for (int i = 0; i < n_greeks; ++i) {
      const std::string greek_name = greek_name_at(greek, i);

      if (greek_name == "vega") {
        result[i] =
          spot * exp_minus_dividend_yield_times_time_to_maturity *
          dnorm_d1 * sqrt_time_to_maturity;
      } else if (greek_name == "gamma") {
        result[i] =
          dnorm_d1 * exp_minus_dividend_yield_times_time_to_maturity /
          (spot * sigma * sqrt_time_to_maturity);
      } else if (greek_name == "vanna") {
        result[i] =
          -exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
          d2 / sigma;
      } else if (greek_name == "vomma") {
        result[i] =
          spot * exp_minus_dividend_yield_times_time_to_maturity *
          dnorm_d1 * sqrt_time_to_maturity * d1 * d2 / sigma;
      } else if (greek_name == "veta") {
        result[i] =
          -spot * exp_minus_dividend_yield_times_time_to_maturity *
          dnorm_d1 * sqrt_time_to_maturity *
          (dividend_yield + ((r - dividend_yield) * d1) /
           (sigma * sqrt_time_to_maturity) -
           ((1.0 + d1 * d2) / (2.0 * time)));
      } else if (greek_name == "vera") {
        result[i] =
          exercise_price * time * exp_minus_r_times_time_to_maturity *
          standard_normal_pdf(d2) * -(sqrt_time_to_maturity + d2 / sigma);
      } else if (greek_name == "speed") {
        result[i] =
          -exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 /
          (spot * spot * sigma * sqrt_time_to_maturity) *
          (d1 / (sigma * sqrt_time_to_maturity) + 1.0);
      } else if (greek_name == "zomma") {
        result[i] =
          exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 *
          (d1 * d2 - 1.0) /
          (spot * sigma * sigma * sqrt_time_to_maturity);
      } else if (greek_name == "color") {
        result[i] =
          -exp_minus_dividend_yield_times_time_to_maturity * dnorm_d1 /
          (2.0 * spot * std::pow(time, 1.5) * sigma) *
          (2.0 * dividend_yield * time + 1.0 +
           (2.0 * (r - dividend_yield) * time -
            d2 * sigma * sqrt_time_to_maturity) /
           (sigma * sqrt_time_to_maturity) * d1);
      } else if (greek_name == "ultima") {
        result[i] =
          (-spot * exp_minus_dividend_yield_times_time_to_maturity *
           dnorm_d1 * sqrt_time_to_maturity) /
          (sigma * sigma) *
          (d1 * d2 * (1.0 - d1 * d2) + d1 * d1 + d2 * d2);
      }
    }

    return result;
  }

  const bool is_asset_or_nothing =
    payoff == "asset_or_nothing_call" || payoff == "asset_or_nothing_put";
  const bool is_call =
    payoff == "cash_or_nothing_call" || payoff == "asset_or_nothing_call";

  // Binary option values all have the form
  //   prefactor * pnorm(normal_argument).
  // For cash-or-nothing options the argument uses d2; for asset-or-nothing
  // options it uses d1. Puts use the negative argument.
  const double normal_sign = is_call ? 1.0 : -1.0;
  const double d_shift = is_asset_or_nothing ? 0.5 : -0.5;
  const double base_d = is_asset_or_nothing ? d1 : d2;
  const double normal_argument = normal_sign * base_d;
  const double normal_cdf = standard_normal_cdf(normal_argument);
  const double normal_pdf = standard_normal_pdf(normal_argument);

  const double prefactor = is_asset_or_nothing ?
    spot * exp_minus_dividend_yield_times_time_to_maturity :
    exp_minus_r_times_time_to_maturity;

  // The formulas below differentiate prefactor * Phi(x), where
  // x is normal_argument and Phi is pnorm. We store derivatives of
  // log(prefactor) and x separately, then combine them generically.
  const double log_prefactor_spot =
    is_asset_or_nothing ? 1.0 / spot : 0.0;
  const double log_prefactor_volatility = 0.0;
  const double log_prefactor_time =
    is_asset_or_nothing ? -dividend_yield : -r;
  const double log_prefactor_r = is_asset_or_nothing ? 0.0 : -time;
  const double log_prefactor_dividend =
    is_asset_or_nothing ? -time : 0.0;

  const double log_prefactor_spot_spot =
    is_asset_or_nothing ? -1.0 / (spot * spot) : 0.0;
  const double log_prefactor_spot_spot_spot =
    is_asset_or_nothing ? 2.0 / (spot * spot * spot) : 0.0;

  const double d_without_volatility_shift =
    base_d - d_shift * sigma * sqrt_time_to_maturity;

  // First, second, and third derivatives of d1/d2 before applying the
  // call/put sign. Only combinations needed by the supported Greeks appear.
  const double d_spot = 1.0 / (spot * sigma * sqrt_time_to_maturity);
  const double d_volatility =
    -base_d / sigma + 2.0 * d_shift * sqrt_time_to_maturity;
  const double d_time =
    (r - dividend_yield + d_shift * sigma * sigma) /
    (sigma * sqrt_time_to_maturity) - base_d / (2.0 * time);
  const double d_r = sqrt_time_to_maturity / sigma;
  const double d_dividend = -sqrt_time_to_maturity / sigma;

  const double d_spot_spot =
    -1.0 / (spot * spot * sigma * sqrt_time_to_maturity);
  const double d_spot_volatility =
    -1.0 / (spot * sigma * sigma * sqrt_time_to_maturity);
  const double d_spot_time =
    -1.0 / (2.0 * spot * sigma * time * sqrt_time_to_maturity);
  const double d_volatility_volatility =
    2.0 * d_without_volatility_shift / (sigma * sigma);
  const double d_volatility_time =
    -d_time / sigma + d_shift / sqrt_time_to_maturity;
  const double d_volatility_r =
    -sqrt_time_to_maturity / (sigma * sigma);

  const double d_spot_spot_spot =
    2.0 / (spot * spot * spot * sigma * sqrt_time_to_maturity);
  const double d_spot_spot_time =
    1.0 / (2.0 * spot * spot * sigma * time * sqrt_time_to_maturity);
  const double d_spot_spot_volatility =
    1.0 / (spot * spot * sigma * sigma * sqrt_time_to_maturity);
  const double d_volatility_volatility_volatility =
    -6.0 * d_without_volatility_shift / (sigma * sigma * sigma);

  // Convert derivatives of d1/d2 into derivatives of the actual normal
  // argument, i.e. d for calls and -d for puts.
  const double normal_spot = normal_sign * d_spot;
  const double normal_volatility = normal_sign * d_volatility;
  const double normal_time = normal_sign * d_time;
  const double normal_r = normal_sign * d_r;
  const double normal_dividend = normal_sign * d_dividend;

  const double normal_spot_spot = normal_sign * d_spot_spot;
  const double normal_spot_volatility = normal_sign * d_spot_volatility;
  const double normal_spot_time = normal_sign * d_spot_time;
  const double normal_volatility_volatility =
    normal_sign * d_volatility_volatility;
  const double normal_volatility_time = normal_sign * d_volatility_time;
  const double normal_volatility_r = normal_sign * d_volatility_r;

  const double normal_spot_spot_spot = normal_sign * d_spot_spot_spot;
  const double normal_spot_spot_volatility =
    normal_sign * d_spot_spot_volatility;
  const double normal_spot_spot_time = normal_sign * d_spot_spot_time;
  const double normal_volatility_volatility_volatility =
    normal_sign * d_volatility_volatility_volatility;

  // If V = A * Phi(x) and L = log(A), then
  //   V_i = A * (L_i * Phi(x) + phi(x) * x_i).
  auto first_derivative = [&](double log_i, double normal_i) {
    return prefactor * (log_i * normal_cdf + normal_pdf * normal_i);
  };

  // Same setup for V_ij. The last term comes from
  // d/dx phi(x) = -x * phi(x).
  auto second_derivative = [&](double log_i,
                               double log_j,
                               double log_ij,
                               double normal_i,
                               double normal_j,
                               double normal_ij) {
    const double cdf_coefficient = log_ij + log_i * log_j;
    const double pdf_coefficient =
      log_i * normal_j + log_j * normal_i + normal_ij;
    const double pdf_derivative_coefficient = normal_i * normal_j;

    return prefactor *
      (cdf_coefficient * normal_cdf +
       pdf_coefficient * normal_pdf -
       pdf_derivative_coefficient * normal_argument * normal_pdf);
  };

  // Differentiate V_ij once more. This is used only for speed, zomma,
  // color, and ultima.
  auto third_derivative = [&](double log_i,
                              double log_j,
                              double log_k,
                              double log_ij,
                              double log_ik,
                              double log_jk,
                              double log_ijk,
                              double normal_i,
                              double normal_j,
                              double normal_k,
                              double normal_ij,
                              double normal_ik,
                              double normal_jk,
                              double normal_ijk) {
    const double cdf_coefficient = log_ij + log_i * log_j;
    const double pdf_coefficient =
      log_i * normal_j + log_j * normal_i + normal_ij;
    const double pdf_derivative_coefficient = normal_i * normal_j;

    const double cdf_coefficient_k =
      log_ijk + log_ik * log_j + log_i * log_jk;
    const double pdf_coefficient_k =
      log_ik * normal_j + log_i * normal_jk +
      log_jk * normal_i + log_j * normal_ik + normal_ijk;
    const double pdf_derivative_coefficient_k =
      normal_ik * normal_j + normal_i * normal_jk;

    return prefactor *
      ((log_k * cdf_coefficient + cdf_coefficient_k) * normal_cdf +
       (log_k * pdf_coefficient + cdf_coefficient * normal_k +
        pdf_coefficient_k) * normal_pdf -
       (log_k * pdf_derivative_coefficient +
        pdf_coefficient * normal_k + pdf_derivative_coefficient_k) *
       normal_argument * normal_pdf +
       pdf_derivative_coefficient * normal_k *
       (normal_argument * normal_argument - 1.0) * normal_pdf);
  };

  const double fair_value = prefactor * normal_cdf;

  for (int i = 0; i < n_greeks; ++i) {
    const std::string greek_name = greek_name_at(greek, i);

    if (greek_name == "fair_value") {
      result[i] = fair_value;
    } else if (greek_name == "delta") {
      result[i] = first_derivative(log_prefactor_spot, normal_spot);
    } else if (greek_name == "vega") {
      result[i] =
        first_derivative(log_prefactor_volatility, normal_volatility);
    } else if (greek_name == "theta") {
      result[i] = -first_derivative(log_prefactor_time, normal_time);
    } else if (greek_name == "rho") {
      result[i] = first_derivative(log_prefactor_r, normal_r);
    } else if (greek_name == "epsilon") {
      result[i] =
        first_derivative(log_prefactor_dividend, normal_dividend);
    } else if (greek_name == "lambda") {
      result[i] =
        first_derivative(log_prefactor_spot, normal_spot) * spot / fair_value;
    } else if (greek_name == "gamma") {
      result[i] =
        second_derivative(
          log_prefactor_spot, log_prefactor_spot,
          log_prefactor_spot_spot,
          normal_spot, normal_spot, normal_spot_spot);
    } else if (greek_name == "vanna") {
      result[i] =
        second_derivative(
          log_prefactor_spot, log_prefactor_volatility, 0.0,
          normal_spot, normal_volatility, normal_spot_volatility);
    } else if (greek_name == "charm") {
      result[i] =
        -second_derivative(
          log_prefactor_spot, log_prefactor_time, 0.0,
          normal_spot, normal_time, normal_spot_time);
    } else if (greek_name == "vomma") {
      result[i] =
        second_derivative(
          log_prefactor_volatility, log_prefactor_volatility, 0.0,
          normal_volatility, normal_volatility,
          normal_volatility_volatility);
    } else if (greek_name == "veta") {
      result[i] =
        second_derivative(
          log_prefactor_volatility, log_prefactor_time, 0.0,
          normal_volatility, normal_time, normal_volatility_time);
    } else if (greek_name == "vera") {
      result[i] =
        second_derivative(
          log_prefactor_r, log_prefactor_volatility, 0.0,
          normal_r, normal_volatility, normal_volatility_r);
    } else if (greek_name == "speed") {
      result[i] =
        third_derivative(
          log_prefactor_spot, log_prefactor_spot, log_prefactor_spot,
          log_prefactor_spot_spot, log_prefactor_spot_spot,
          log_prefactor_spot_spot, log_prefactor_spot_spot_spot,
          normal_spot, normal_spot, normal_spot,
          normal_spot_spot, normal_spot_spot, normal_spot_spot,
          normal_spot_spot_spot);
    } else if (greek_name == "zomma") {
      result[i] =
        third_derivative(
          log_prefactor_spot, log_prefactor_spot,
          log_prefactor_volatility,
          log_prefactor_spot_spot, 0.0, 0.0, 0.0,
          normal_spot, normal_spot, normal_volatility,
          normal_spot_spot, normal_spot_volatility,
          normal_spot_volatility, normal_spot_spot_volatility);
    } else if (greek_name == "color") {
      result[i] =
        third_derivative(
          log_prefactor_spot, log_prefactor_spot, log_prefactor_time,
          log_prefactor_spot_spot, 0.0, 0.0, 0.0,
          normal_spot, normal_spot, normal_time,
          normal_spot_spot, normal_spot_time, normal_spot_time,
          normal_spot_spot_time);
    } else if (greek_name == "ultima") {
      result[i] =
        third_derivative(
          log_prefactor_volatility, log_prefactor_volatility,
          log_prefactor_volatility,
          0.0, 0.0, 0.0, 0.0,
          normal_volatility, normal_volatility, normal_volatility,
          normal_volatility_volatility, normal_volatility_volatility,
          normal_volatility_volatility,
          normal_volatility_volatility_volatility);
    }
  }

  return result;
}
