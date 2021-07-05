#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_I_3(NumericMatrix X, int steps, float dt) {
  int i;
  NumericVector I;
  double dt4 = dt*dt*dt*dt;
  I = X(_, steps)/2 * steps * dt4;
  for(i = 1; i < steps; i++) {
    I = I + X(_, i) * i*i*i * dt4;
  }
  return I;
}
