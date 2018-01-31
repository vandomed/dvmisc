#include <Rcpp.h>
using namespace Rcpp;

//' True Range of Numeric Values
//' 
//' Defined as the difference between the maximum and the minimum. Equivalent to 
//' base R code \code{diff(range(x))}, but much faster. For integer objects, 
//' \code{\link{true_range_i}} should run even faster.
//' 
//' @param x Numeric vector or matrix.
//' 
//' @return Numeric value.
//' 
//' @examples 
//' # In general, true_range_n is much faster than diff(range(x))
//' x <- rnorm(1000)
//' all.equal(diff(range(x)), true_range_n(x))
//' benchmark(diff(range(x)), true_range_n(x), replications = 5000)
//' 
//' # For integer vectors, true_range_i should be even faster
//' x <- rpois(1000, lambda = 5)
//' all.equal(diff(range(x)), true_range_i(x))
//' benchmark(diff(range(x)), true_range_n(x), true_range_i(x), 
//'           replications = 5000)
//' 
//' @export
// [[Rcpp::export]]
double true_range_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  double maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  double out = maxx - minx;
  return(out);
}