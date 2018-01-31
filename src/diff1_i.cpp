#include <Rcpp.h>
using namespace Rcpp;

//' 1-Unit Lagged Differences for Integer Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{diff}} for calculating differences between adjacent values 
//' of an integer vector.
//' 
//' @param x Integer vector.
//' 
//' @return Integer vector.
//' 
//' @examples 
//' # diff1_i is typically much faster than diff
//' x <- rpois(1000, lambda = 5)
//' all.equal(diff(x), diff1_i(x))
//' benchmark(diff(x), diff1_i(x), replications = 2000)
//' 
//' @export
// [[Rcpp::export]]
IntegerVector diff1_i(IntegerVector x) {
  int n = x.size();
  int n_less1 = n - 1;
  IntegerVector out(n_less1);
  for (int a = 0; a < n_less1; ++a) {
    out[a] = x[a + 1] - x[a];
  }
  return(out);
}