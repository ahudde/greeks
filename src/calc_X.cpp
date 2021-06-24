#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector calc_X(NumericMatrix W, int steps, int paths, float dt,
                     float initial_price, float volatility, float r) {
  int i;
  NumericMatrix X(paths, steps + 1);
  for(i = 0; i <= steps; i++) {
    X(_, i) = initial_price *
      exp((r - volatility*volatility/2) * i*dt + (volatility* W(_, i)));
  }
  return X;
}
