#include <Rcpp.h>
using namespace Rcpp;

//' 1-Unit Lagged Differences for Numeric Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{diff}} for calculating differences between adjacent values 
//' of a numeric vector. For integer vectors, \code{\link{diff1_i}} should run 
//' even faster. 
//' 
//' @param x Numeric vector.
//' 
//' @return Numeric vector.
//' 
//' @examples 
//' # In general, diff1_n is much faster than diff
//' x <- rnorm(1000)
//' all.equal(diff(x), diff1_n(x))
//' benchmark(diff(x), diff1_n(x), replications = 3000)
//' 
//' # For integer vectors, diff1_i should be even faster
//' x <- rpois(1000, lambda = 5)
//' all.equal(diff(x), diff1_i(x))
//' benchmark(diff(x), diff1_n(x), diff1_i(x), replications = 3000)
//' 
//' @export
// [[Rcpp::export]]
NumericVector diff1_n(NumericVector x) {
  int n = x.size();
  int n_less1 = n - 1;
  NumericVector out(n_less1);
  for (int a = 0; a < n_less1; ++a) {
    out[a] = x[a + 1] - x[a];
  }
  return(out);
}