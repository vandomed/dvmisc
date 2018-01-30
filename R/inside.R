#' Check Whether Numeric Value Falls Inside Two Other Numeric Values
#' 
#' Returns \code{TRUE} if \code{x} falls inside range defined by \code{ends}
#' and \code{FALSE} otherwise. Also works for multiple sets of values and/or
#' endpoints.
#' 
#' @param x Numeric value or vector of numeric values.
#' @param ends Numeric vector of length 2 specifying the endpoints for the 
#' interval, or a 2-column numeric matrix where each row specifies a pair of 
#' endpoints.
#' @param inclusive Logical value indicating whether endpoints should be 
#' included.
#' @param include.lower Logical value indicating whether the lower endpoint 
#' should be included.
#' @param include.upper Logical value indicating whether the lower endpoint 
#' should be included.
#' 
#' @return Logical value or vector.
#' 
#' @examples
#' # Check whether 2 is inside [0, 2.5]
#' inside(1, c(0, 2.5))
#' 
#' # Check whether 2 and 3 are inside (0, 3)
#' inside(c(2, 3), c(0, 3), inclusive = FALSE)
#' 
#' # Check whether 1 is inside [1, 2] and [3, 4]
#' inside(1, rbind(c(1, 2), c(3, 4)))
#' 
#' @export
inside <- function(x, ends, inclusive = TRUE, include.lower = inclusive,
                   include.upper = inclusive) {
  
  # Get >=/> and <=/< depending on values of include.lower and include.upper
  sign1 <- ifelse(include.lower, ">=", ">")
  sign2 <- ifelse(include.upper, "<=", "<")
  
  # Check whether x is inside specified interval(s)
  if (! is.matrix(ends)) {
    
    out <- sapply(x, function(x)
      eval(parse(text = paste(x, sign1, ends[1], "&", x, sign2, ends[2]))))
    
  } else {
    
    mat <- cbind(x, ends)
    out <- apply(mat, 1, function(x) {
      eval(parse(text = paste(x[1], sign1, x[2], "&", x[1], sign2, x[3])))
    })
    
  }
  
  # Return logical
  return(out)
  
}