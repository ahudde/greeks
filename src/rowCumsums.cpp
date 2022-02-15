#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rowCumsums(NumericMatrix m) {

  int i;

  for(i = 1; i < m.ncol(); i++) {
    m(_, i) = m(_, i-1) + m(_, i);
  }
  return m;
}
