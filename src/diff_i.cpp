#include <Rcpp.h>
using namespace Rcpp;

//' Lagged Differences for Integer Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{diff}} for calculating lagged differences for an integer 
//' vector.
//' 
//' @param x Integer vector.
//' @param lag Integer value.
//' 
//' @return Integer vector.
//' 
//' @examples 
//' # diff_i is typically much faster than diff
//' x <- rpois(1000, lambda = 5)
//' all.equal(diff(x, 2), diff_i(x, 2))
//' benchmark(diff(x, 2), diff_i(x, 2), replications = 2000)
//' 
//' @export
// [[Rcpp::export]]
IntegerVector diff_i(IntegerVector x, int lag = 1) {
  int n = x.size();
  int n_lesslag = n - lag;
  IntegerVector out(n_lesslag);
  for (int a = 0; a < n_lesslag; ++a) {
    out[a] = x[a + lag] - x[a];
  }
  return(out);
}