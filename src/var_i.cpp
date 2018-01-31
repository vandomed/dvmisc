#include <Rcpp.h>
using namespace Rcpp;

//' Sample Variance for Integer Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[stats]{var}} for integer vectors. Not valid for non-integer 
//' input vectors.
//' 
//' @param x Integer vector.
//' 
//' @return Numeric value.
//' 
//' @examples 
//' # For integer vectors, var_i is typically much faster than var.
//' x <- rpois(1000, lambda = 5)
//' all.equal(var(x), var_i(x))
//' benchmark(var(x), var_i(x), replications = 5000)
//' 
//' @export
// [[Rcpp::export]]
double var_i(IntegerVector x) {
  double n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumx2 = 0;
  for (int a = 0; a < n; ++a) {
    sumx += x[a];
    sumx2 += std::pow(x[a], 2);
  }
  double s2 = 1 / nless1 * (sumx2 - n * std::pow(sumx / n, 2));
  return(s2);
}