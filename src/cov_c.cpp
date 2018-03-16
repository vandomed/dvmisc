#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cov_i(IntegerVector x, IntegerVector y) {
  
  // Get length(x) and initialize variables
  double n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumy = 0;
  double sumxy = 0;
  double xa = 0;
  double ya = 0;
  
  // Loop through and calculate products
  for (int a = 0; a < n; ++a) {
    xa = x[a];
    ya = y[a];
    sumx += xa;
    sumy += ya;
    sumxy += xa * ya;
  }
  double covxy = 1 / nless1 * (sumxy - sumx * sumy / n);
  return(covxy);
  
}

// [[Rcpp::export]]
double cov_n(NumericVector x, NumericVector y) {
  
  // Get length(x) and initialize variables
  double n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumy = 0;
  double sumxy = 0;
  double xa, ya;
  
  // Loop through and calculate products
  for (int a = 0; a < n; ++a) {
    xa = x[a];
    ya = y[a];
    sumx += xa;
    sumy += ya;
    sumxy += xa * ya;
  }
  double covxy = 1 / nless1 * (sumxy - sumx * sumy / n);
  return(covxy);
  
}