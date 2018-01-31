#include <Rcpp.h>
using namespace Rcpp;

//' Range (Actually Minimum and Maximum) of Numeric Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{range}} for numeric vectors/matrices. For integer objects, 
//' \code{\link{range_i}} should run even faster. 
//' 
//' @param x Numeric vector or matrix.
//' 
//' @return Numeric vector.
//' 
//' @examples 
//' # In general, range_n is much faster than range
//' x <- rnorm(1000)
//' all.equal(range(x), range_n(x))
//' benchmark(range(x), range_n(x), replications = 5000)
//' 
//' # For integer vectors, range_i should be even faster
//' x <- rpois(1000, lambda = 5) 
//' all.equal(range(x), range_i(x))
//' benchmark(range(x), range_n(x), range_i(x), replications = 10000)
//' 
//' @export
// [[Rcpp::export]]
NumericVector range_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  double maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  NumericVector out(2);
  out[0] = minx;
  out[1] = maxx;
  return(out);
}