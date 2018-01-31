#' Sample Standard Deviation for Numeric Values
#' 
#' Written in C++, this function should always run faster than 
#' \code{\link[stats]{sd}} for numeric vectors. For integer vectors, 
#' \code{\link{sd_i}} should run even faster.
#' 
#' @param x Numeric vector.
#' 
#' @return Numeric value.
#' 
#' @examples
#' # In general, sd_n is much faster than sd.
#' x <- rnorm(1000)
#' all.equal(sd(x), sd_n(x))
#' benchmark(sd(x), sd_n(x), replications = 2000) 
#' 
#' # For integer vectors, sd_i should be even faster.
#' x <- rpois(1000, lambda = 5)
#' all.equal(sd(x), sd_i(x))
#' benchmark(sd(x), sd_n(x), sd_i(x), replications = 2000)
#' 
#' @export
sd_n <- function(x) {
  sqrt(.Call(`_dvmisc_var_n`, x))
}