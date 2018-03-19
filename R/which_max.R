#' Return Index of (First) Maximum of a Vector
#' 
#' Returns index of maximum for vectors and index or (row, column) position for 
#' matrices. For optimal speed, use \code{integer = TRUE} if \code{x} is an 
#' integer vector/matrix and \code{integer = FALSE} otherwise. Typically faster 
#' than \code{\link[base]{which.max}} for matrices and for large vectors.
#' 
#' 
#' @param x Integer or numeric vector.
#' @param integer Logical value for whether \code{x} is an integer vector.
#' 
#' 
#' @return Numeric value.
#' 
#' 
#' @references
#' Papadakis, M., Tsagris, M., Dimitriadis, M., Fafalios, S., Tsamardinos, I., 
#' Fasiolo, M., Borboudakis, G., Burkardt, J., Zou, C. and Lakiotaki, K. (2018) 
#' Rfast: A Collection of Efficient and Extremely Fast R Functions. R package 
#' version 1.8.8. \url{https://CRAN.R-project.org/package=Rfast}.
#' 
#' 
#' @examples
#' # var2 vs. var for integer vector
#' x <- rpois(1000, lambda = 5)
#' all.equal(var(x), var2(x, TRUE))
#' benchmark(var(x), var2(x, TRUE), replications = 2000)
#' 
#' # var2 vs. var for numeric vector
#' x <- rnorm(1000)
#' all.equal(var(x), var2(x))
#' benchmark(var(x), var2(x), replications = 2000)
#' 
#' 
#' @export
var2 <- function(x, integer = FALSE) {
  if (integer) {
    return(.Call(`_crowdopt_var_i`, x))
  }
  return(.Call(`_crowdopt_var_n`, x))
}