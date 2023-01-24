#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_X(NumericMatrix W, float dt, float volatility, float r) {
  int i;
  NumericMatrix X(W.nrow(), W.ncol());
  for(i = 0; i < W.ncol(); i++) {
    X(_, i) = exp((r - volatility*volatility/2) * i*dt + (volatility* W(_, i)));
  }
  return X;
}
