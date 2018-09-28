#' Same as expand.grid but with Sequences Reversed
#' 
#' Loops over the last argument, then the second-last, and so on. Defined simply 
#' as \code{rev(expand.grid(rev(list(...))))}. 
#' 
#' @param ... See \code{\link[base]{expand.grid}}.
#' @param KEEP.OUT.ATTRS See \code{\link[base]{expand.grid}}.
#' @param stringsAsFactors See \code{\link[base]{expand.grid}}.
#' 
#' @return Data frame.
#' 
#' @examples
#' # Compare expand.grid to expand_grid
#' expand.grid(x = c("a", "b", "c"), y = c(1, 2), z = c(TRUE, FALSE))
#' expand_grid(x = c("a", "b", "c"), y = c(1, 2), z = c(TRUE, FALSE))
#' 
#' @export
expand_grid <- function(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE) {
  rev(expand.grid(rev(list(...)), 
                  KEEP.OUT.ATTRS = KEEP.OUT.ATTRS, 
                  stringsAsFactors = stringsAsFactors))
}