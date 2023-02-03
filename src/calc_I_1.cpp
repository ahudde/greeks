#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_I_1(NumericMatrix X, int steps, float dt) {
  int i;
  int paths = X.nrow();

  NumericVector I(paths);
  I = X(_, steps)/2 * (steps * dt);
  for(i = 1; i < steps; i++) {
    I = I + X(_, i) * (i * dt);
  }
  I = I * dt;
  return I;
}
