#' Mean of Numeric Values
#' 
#' Defined simply as \code{sum(x) / length(x)}, this function seems to always 
#' run faster than \code{\link[base]{mean}} for numeric, non-integer 
#' vectors/matrices. For integer objects, \code{\link{mean_i}} should run even 
#' faster.
#' 
#' @param x Numeric vector or matrix.
#' 
#' @return Numeric value.
#' 
#' @examples 
#' # In general, mean_n is much faster than mean.
#' x <- rnorm(10000)
#' mean(x) == mean_n(x)
#' benchmark(mean(x), mean_n(x), replications = 1000)
#' 
#' # For very large integer objects, mean may be faster than mean_n. But then 
#' # mean_i should be even faster.
#' x <- rpois(100000, lambda = 5)
#' mean(x) == mean_n(x)
#' mean(x) == mean_i(x)
#' benchmark(mean(x), mean_n(x), mean_i(x), replications = 1000)
#' 
#' @export
mean_n <- function(x) {
  out <- sum(x) / length(x)
  return(out)
}