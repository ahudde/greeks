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

  NumericVector result(3);

  result.names() = StringVector({"fair_value", "delta", "gamma"});

  double dt = time_to_maturity/steps;

  double up = exp(volatility * sqrt(dt));
  double down = exp(-volatility * sqrt(dt));
  double p = (exp((r-dividend_yield)*dt) - down)/(up-down);
  double p_ = exp(-r*dt)*p;
  double q_ = exp(-r*dt)*(1-p);

  // generate the price vector

  NumericVector price(2*steps+1);

  for(j = 0; j <= 2*steps; j++) {
    price(j) = initial_price * exp(-volatility * (steps - j) * sqrt(dt));
  }

  // generate the value matrix

  NumericVector value(steps+1);

  if(payoff == "call") {
    payoff_function = &american_call;
  } else if(payoff == "put") {
    payoff_function = &american_put;
    }

  for(i = 0; i <= steps; i++) {
      value(i) = payoff_function(0.0, price(2*steps - 2*i), exercise_price);
    }

  for(j = steps-1; j >= 1; j--) {
    for(i = 0; i <= j; i++) {
      value(i) = payoff_function(p_*value(i) + q_*value(i+1),
            price(2*steps - 2*i + j - steps), exercise_price);
    }
  }

  result("fair_value") = max(p_*value(0) + q_*value(1),
         price(2*steps + j - steps) - exercise_price);
  result("delta") = (value(0) - value(1)) / (initial_price * (up - down));
  result("gamma") = (value(0) - 2*result("fair_value") + value(1)) /
    (initial_price * (up - down) * (up - down));

  return result;

}
