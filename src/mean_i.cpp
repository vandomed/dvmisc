#include <Rcpp.h>
using namespace Rcpp;

//' Mean of Integer Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[base]{mean}} for integer vectors/matrices. Not valid for 
//' non-integer objects.
//' 
//' @param x Integer vector or matrix.
//' 
//' @return Numeric value.
//' 
//' @examples 
//' # For integer objects, mean_i is typically much faster than mean.
//' x <- rpois(100, lambda = 5)
//' mean(x) == mean_i(x)
//' benchmark(mean(x), mean_i(x), replications = 10000)
//' 
//' @export
// [[Rcpp::export]]
double mean_i(IntegerVector x) {
  int n = x.size();
  double sumx = 0;
  for (int a = 0; a < n; ++a) {
    sumx += x[a];
  }
  double meanx = sumx / n;
  return(meanx);
}
