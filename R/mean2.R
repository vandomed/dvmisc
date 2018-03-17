#' Arithmetic Mean
#' 
#' Faster version of base R function \code{\link[base]{mean}}. For optimal speed, 
#' use \code{Integer = TRUE} if \code{x} is an integer vector and 
#' \code{integer = FALSE} otherwise. 
#' 
#' @param x Integer or numeric vector.
#' @param integer Logical value for whether \code{x} is an integer vector.
#' 
#' @return Numeric value.
#' 
#' @examples 
#' # mean2 vs. mean for integer vector
#' x <- rpois(1000, lambda = 5)
#' all.equal(mean(x), mean2(x, TRUE))
#' benchmark(mean(x), mean2(x, TRUE), replications = 1000)
#' 
#' # mean2 vs. mean for numeric vector
#' x <- rnorm(1000)
#' all.equal(mean(x), mean2(x))
#' benchmark(mean(x), mean2(x), replications = 1000)
#' 
#' 
#' @export
mean2 <- function(x, integer = FALSE) {
  if (integer) {
    return(.Call(`_dvmisc_mean_i`, x))
  }
  return(sum(x) / length(x))
}