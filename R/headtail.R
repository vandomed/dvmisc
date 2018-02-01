#' Return the First and Last Part of an Object
#' 
#' # Simply \code{\link[utils]{head}} and \code{\link[utils]{tail}} combined 
#' into one function.
#' 
#' @param x Input object.
#' @param ... Additional arguments to pass to \code{\link[utils]{head}} and 
#' \code{\link[utils]{tail}} functions.
#' 
#' @return Same class as \code{x}.
#' 
#' @examples
#' # Generate data from N(0, 1), sort, and look at smallest and largest 3 values
#' x <- rnorm(1000)
#' x.sorted <- sort(x)
#' headtail(x.sorted, 3)
#' 
#' @export
headtail <- function(x, ...) {
  
  # Determine class of x
  class.x <- class(x)
  
  # Create output object according to class of x
  if (class.x %in% c("numeric", "character", "logical", "list")) {
    y <- c(head(x, ...), tail(x, ...))
  } else if (class.x %in% c("matrix", "data.frame")) {
    y <- rbind(head(x, ...), tail(x, ...))
  }
  
  # Return head/tail object
  return(y)
  
}