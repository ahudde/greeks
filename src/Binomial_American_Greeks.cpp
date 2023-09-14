#include <Rcpp.h>
#include <string>

using namespace Rcpp;

using std::string;
using std::max;


double american_call(double x, double price, double exercise_price) {
  return max(x, price - exercise_price);
}

double american_put(double x, double price, double exercise_price) {
  return max(x, exercise_price - price);
}

// [[Rcpp::export]]
NumericVector Binomial_American_Greeks_cpp(double initial_price = 100,
                                           double exercise_price = 100,
                                           double r = 0,
                                           double time_to_maturity = 1,
                                           double volatility = 0.3,
                                           double dividend_yield = 0,
                                           std::string payoff = "call",
                                           const int steps = 1000) {

  // payoff_function
  double (*payoff_function)(double x, double price, double exercise_price);

  // iterators
  int i;
  int j;

  NumericVector result(2);

  result.names() =
    StringVector({"fair_value", "european_fair_value"});

  const double dt = time_to_maturity/steps;
  const double up = exp(volatility * sqrt(dt));
  const double down = exp(-volatility * sqrt(dt));
  const double p = (exp((r-dividend_yield)*dt) - down)/(up-down);
  const double q = (1 - p);
  const double exp_min_r_dt = exp(-r*dt);

  // generate the price vector
  NumericVector price(2*steps+1);

  for(j = 0; j <= 2*steps; j++) {
    price(j) = initial_price * exp(-volatility * (steps - j) * sqrt(dt));
  }

  // generate the value matrix
  NumericVector american_value(steps+1);
  NumericVector european_value(steps+1);

  if (payoff == "call") {
    payoff_function = &american_call;
  } else if (payoff == "put") {
    payoff_function = &american_put;
  }

  for(i = 0; i <= steps; i++) {
    american_value(i) = payoff_function(0.0, price(2*steps - 2*i), exercise_price);
  }

  european_value = american_value;

  for(j = steps-1; j >= 0; j--) {
    for(i = 0; i <= j; i++) {
      american_value(i) =
        payoff_function(
          exp_min_r_dt * (p*american_value(i) + q*american_value(i+1)),
          price(2*steps - 2*i + j - steps), exercise_price);
      european_value(i) = p * european_value(i) + q * european_value(i);
    }
  }

  result("fair_value") = american_value(0);
  result("european_fair_value") = european_value(0);

  return result;

}
