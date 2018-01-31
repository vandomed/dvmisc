#include <Rcpp.h>
using namespace Rcpp;

//' Sample Variance for Numeric Values
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[stats]{var}} for numeric vectors. For integer vectors, 
//' \code{\link{var_i}} should run even faster.
//' 
//' @param x Numeric vector.
//' 
//' @return Numeric value.
//' 
//' @examples 
//' # In general, var_n is much faster than var.
//' x <- rnorm(1000)
//' all.equal(var(x), var_n(x))
//' benchmark(var(x), var_n(x), replications = 1000)
//' 
//' # For integer vectors, var_i should be even faster.
//' x <- rpois(1000, lambda = 5)
//' all.equal(var(x), var_i(x))
//' benchmark(var(x), var_n(x), var_i(x), replications = 1000)
//' 
//' @export
// [[Rcpp::export]]
double var_n(NumericVector x) {
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