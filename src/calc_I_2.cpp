#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector calc_I_2(NumericMatrix X, int steps, float dt) {
  int i;
  NumericVector I;
  I = X(_, steps)/2 * steps * dt * steps * dt * dt;
  for(i = 1; i < steps; i++) {
    I = I + X(_, i) * (i * dt) * (i * dt) * dt;
  }
  return I;
}
