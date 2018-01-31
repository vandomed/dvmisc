#include <Rcpp.h>
using namespace Rcpp;

//' Range (Actually Minimum and Maximum) of Integer Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{range}} for integer vectors/matrices. Not valid for 
//' non-integer objects.
//' 
//' @param x Integer vector or matrix.
//' 
//' @return Integer vector.
//' 
//' @examples 
//' # In general, range_i is much faster than range
//' x <- rpois(1000, lambda = 5) 
//' all.equal(range(x), range_i(x))
//' benchmark(range(x), range_i(x), replications = 10000)
//' 
//' @export
// [[Rcpp::export]]
IntegerVector range_i(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int minx = currentx;
  int maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  IntegerVector out(2);
  out[0] = minx;
  out[1] = maxx;
  return(out);
}