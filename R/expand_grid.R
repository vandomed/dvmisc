#' Same as expand.grid but with Sequences Reversed
#' 
#' Loops over the last argument, then the second-last, and so on. Should be 
#' faster than \code{\link[base]{expand.grid}}.
#' 
#' @param ... Vectors you want all combinations of.
#' 
#' @return Data frame.
#' 
#' @examples
#' # Compare expand.grid to expand_grid
#' expand.grid(x = c("a", "b", "c"), y = c(1, 2), z = c(TRUE, FALSE))
#' expand_grid(x = c("a", "b", "c"), y = c(1, 2), z = c(TRUE, FALSE))
#' 
#' @export
expand_grid <- function(...) {
  inputs.list <- list(...)
  levels <- vapply(inputs.list, length, integer(1))
  nrows <- prod(levels)
  nreps <- nrows / cumprod(levels)
  df <- mapply(
    FUN = function(x, y, z) {
      rep(rep(x, each = y), z)
    },  
    x = inputs.list, 
    y = nreps, 
    z = rev(nreps), 
    SIMPLIFY = FALSE
  )
  setattr(df, "class", "data.table")
  return(df)
}