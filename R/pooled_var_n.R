#' Pooled Sample Variance for Numeric Vectors
#' 
#' Calculates pooled sample variance used in equal variance two-sample t-test. 
#' For integer vectors, \code{\link{pooled_var_i}} will run faster.
#' 
#' @param x Numeric vector.
#' @param y Numeric vector.
#' 
#' @return Numeric value.
#'
#' @export
pooled_var_n <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  s2 <- ((n1 - 1) * .Call('_dvmisc_var_n', x) +
           (n2 - 1) * .Call('_dvmisc_var_n', y)) /
    (n1 + n2 - 2)
  return(s2)
}