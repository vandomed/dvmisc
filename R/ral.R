#' Remove All Labels
#' 
#' Calls \code{\link[sjlabelled]{remove_all_labels}} and then restores special 
#' variable names.
#'
#' @param x Vector or data frame.
#'
#' @return Vector or data frame.
#'
#' @export
ral <- function(x) {
  
  variable.names <- names(x)
  y <- remove_all_labels(x)
  names(y) <- variable.names
  return(y)
  
}
