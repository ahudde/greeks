#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix make_BM(NumericVector v, int steps, int paths) {

  int col;
  int s = 0;

  NumericMatrix W(paths, steps + 1);

  W(_, 0) = W(_, 0) * 0;

  for(col = 1; col < steps+1; col++) {
    W(_, col) = v[Range(s, s + paths)] + W(_, col - 1);
    s += paths;
  }

  return W;
}
