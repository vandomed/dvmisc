#' Sample Standard Deviation
#' 
#' Faster version of base R function \code{\link[stats]{sd}}. For optimal speed, 
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
#' # sd2 vs. sd for integer vector
#' x <- rpois(1000, lambda = 5)
#' all.equal(sd(x), sd2(x, integer = TRUE))
#' benchmark(sd(x), sd2(x, integer = TRUE), replications = 2000)
#' 
#' # sd2 vs. sd for numeric vector
#' x <- rnorm(1000)
#' all.equal(sd(x), sd2(x))
#' benchmark(sd(x), sd2(x), replications = 2000) 
#' 
#' 
#' @export
sd2 <- function(x, integer = FALSE) {
  if (integer) {
    return(sqrt(.Call(`_dvmisc_var_i`, x)))
  }
  return(sqrt(.Call(`_dvmisc_var_n`, x)))
}