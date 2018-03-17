#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double var_i(IntegerVector x) {
  
  // Get length(x) and initialize variables
  int n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumx_squared = 0;
  
  // Loop through and calculate sums and squared sums
  for (int a = 0; a < n; ++a) {
    double x_a = x(a);
    sumx += x_a;
    sumx_squared += x_a * x_a;
  }
  double s2 = 1 / nless1 * (sumx_squared - sumx * sumx / n);
  return s2;
}

// [[Rcpp::export]]
double var_n(NumericVector x) {
  
  // Get length(x) and initialize variables
  int n = x.size();
  double nless1 = n - 1;
  double sum_x = 0;
  double sum_xsquared = 0;
  
  // Loop through and calculate sums and squared sums
  for (int a = 0; a < n; ++a) {
    double x_a = x(a);
    sum_x += x_a;
    sum_xsquared += x_a * x_a;
  }
  double s2 = 1 / nless1 * (sum_xsquared - sum_x * sum_x / n);
  return s2;
  
}