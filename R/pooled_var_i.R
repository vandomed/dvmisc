#' Pooled Sample Variance for Integer Vectors
#' 
#' Calculates pooled sample variance used in equal variance two-sample t-test. 
#' Runs faster than \code{\link{pooled_var_n}} when \code{x} and \code{y} are 
#' integer vectors, but not valid if \code{x} or \code{y} are non-integer 
#' vectors.
#' 
#' @param x Integer vector.
#' @param y Integer vector.
#' 
#' @return Numeric value.
#'
#' @export
pooled_var_i <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  s2 <- ((n1 - 1) * .Call('_dvmisc_var_i', x) +
           (n2 - 1) * .Call('_dvmisc_var_i', y)) /
    (n1 + n2 - 2)
  return(s2)
}