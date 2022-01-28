#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_XW(NumericMatrix X, NumericMatrix W, int steps, int paths,
                       float dt) {
  int i;
  NumericVector XW(paths);
  XW = X(_, steps)*W(_, steps)/2;
  for(i = 1; i < steps; i++) {
    XW += X(_, i)*W(_, i);
  }
  XW = XW * dt;
  return XW;
}
