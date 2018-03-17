#' Sample Variance
#' 
#' Faster version of base R function \code{\link[stats]{var}}. For optimal speed, 
#' use \code{Integer = TRUE} if \code{x} is an integer vector and 
#' \code{integer = FALSE} otherwise. 
#' 
#' 
#' @param x Integer or numeric vector.
#' @param integer Logical value for whether \code{x} is an integer vector.
#' 
#' 
#' @return Numeric value.
#' 
#' 
#' @examples
#' # var2 vs. var for integer vector
#' x <- rpois(1000, lambda = 5)
#' all.equal(var(x), var2(x, TRUE))
#' benchmark(var(x), var2(x, TRUE), replications = 1000)
#' 
#' # var2 vs. var for numeric vector
#' x <- rnorm(1000)
#' all.equal(var(x), var2(x))
#' benchmark(var(x), var2(x), replications = 1000)
#' 
#' 
#' @export
var2 <- function(x, integer = FALSE) {
  if (integer) {
    return(.Call(`_dvmisc_var_i`, x))
  }
  return(.Call(`_dvmisc_var_n`, x))
}