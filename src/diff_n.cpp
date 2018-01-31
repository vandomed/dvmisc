#include <Rcpp.h>
using namespace Rcpp;

//' Lagged Differences for Numeric Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{diff}} for calculating lagged differences for a numeric 
//' vector. For integer vectors, \code{\link{diff_i}} should run even faster. 
//' even faster. 
//' 
//' @param x Numeric vector.
//' @param lag Integer value.
//' 
//' @return Numeric vector.
//' 
//' @examples 
//' # In general, diff_n is much faster than diff
//' x <- rnorm(1000)
//' all.equal(diff(x, 2), diff_n(x, 2))
//' benchmark(diff(x, 2), diff_n(x, 2), replications = 2000)
//' 
//' # For integer vectors, diff_i should be even faster
//' x <- rpois(1000, lambda = 5)
//' all.equal(diff(x, 2), diff_i(x, 2))
//' benchmark(diff(x, 2), diff_n(x, 2), diff_i(x, 2), replications = 2000)
//' 
//' @export
// [[Rcpp::export]]
NumericVector diff_n(NumericVector x, int lag = 1) {
  int n = x.size();
  int n_lesslag = n - lag;
  NumericVector out(n_lesslag);
  for (int a = 0; a < n_lesslag; ++a) {
    out[a] = x[a + lag] - x[a];
  }
  return(out);
}