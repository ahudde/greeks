#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_tXW(NumericMatrix X, NumericMatrix W, int steps, int paths,
                       float dt) {
  int i;
  NumericVector tXW(paths);
  tXW = X(_, steps)*W(_, steps)/2 * (steps*dt) * dt;
  for(i = 1; i < steps; i++) {
    tXW += X(_, i)*W(_, i) * (i*dt) * dt;
  }
  return tXW;
}
