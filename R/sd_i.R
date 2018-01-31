#' Sample Standard Deviation for Integer Values
#' 
#' Written in C++, this function should always run faster than 
#' \code{\link[stats]{sd}} for integer vectors. Not valid for non-integer 
#' vectors.
#' 
#' @param x Integer vector.
#' 
#' @return Numeric value.
#' 
#' @examples
#' # For integer vectors, var_i is typically much faster than var.
#' x <- rpois(1000, lambda = 5)
#' all.equal(sd(x), sd_i(x))
#' benchmark(sd(x), sd_i(x), replications = 2000)
#' 
#' @export
sd_i <- function(x) {
  sqrt(.Call(`_dvmisc_var_i`, x))
}