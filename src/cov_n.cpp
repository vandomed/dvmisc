#include <Rcpp.h>
using namespace Rcpp;

//' Sample Covariance for Numeric Vectors
//' 
//' Written in C++, this function should always run faster than 
//' \code{\link[stats]{cov}} for numeric vectors. For integer vectors, 
//' \code{\link{cov_i}} should run even faster.
//' 
//' @param x,y Numeric vector.
//' 
//' @return Numeric value.
//' 
//' @examples 
//' # In general, cov_n is much faster than cov
//' x <- rnorm(1000)
//' y <- rnorm(1000)
//' all.equal(cov(x, y), cov_n(x, y))
//' benchmark(cov(x, y), cov_n(x, y), replications = 5000)
//' 
//' # For integer vectors, cov_i should be even faster.
//' x <- rpois(1000, lambda = 5)
//' y <- rpois(1000, lambda = 5)
//' all.equal(cov(x, y), cov_i(x, y))
//' benchmark(cov(x, y), cov_n(x, y), cov_i(x, y), replications = 5000)
//' 
//' @export
// [[Rcpp::export]]
double cov_n(NumericVector x, NumericVector y) {
  double n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumy = 0;
  double sumxy = 0;
  double xa, ya;
  for (int a = 0; a < n; ++a) {
    xa = x[a];
    ya = y[a];
    sumx += xa;
    sumy += ya;
    sumxy += xa * ya;
  }
  double covxy = 1 / nless1 * (sumxy - sumx * sumy / n);
  return(covxy);
}