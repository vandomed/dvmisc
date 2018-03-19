#' Pooled Sample Variance
#' 
#' Calculates pooled sample variance used in equal variance two-sample t-test. 
#' For optimal speed, use \code{integer = TRUE} if \code{x} is an integer vector and 
#' \code{integer = FALSE} otherwise.
#' 
#' @param x,y Integer or numeric vectors.
#' @param integer Logical value for whether \code{x} and \code{y} are integer 
#' vectors.
#' 
#' @return Numeric value.
#'
#' @export
pooled_var <- function(x, y, integer = FALSE) {
  n1 <- length(x)
  n2 <- length(y)
  if (integer) {
    return(((n1 - 1) * var2(x, TRUE) + (n2 - 1) * var2(y, TRUE)) /
    (n1 + n2 - 2))
  }
  return(((n1 - 1) * var2(x) + (n2 - 1) * var2(y)) / (n1 + n2 - 2))
}