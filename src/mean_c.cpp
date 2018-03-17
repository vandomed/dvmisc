#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mean_i(IntegerVector x) {
  int n = x.size();
  double sum_x = 0;
  for (int a = 0; a < n; ++a) {
    sum_x += x[a];
  }
  double mean_x = sum_x / n;
  return(mean_x);
}
