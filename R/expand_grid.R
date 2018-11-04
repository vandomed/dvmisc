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

inputs.list <- list(v1 = c(1, 2), 
                    v2 = c("a", "b", "c"), 
                    v3 = matrix(1:6, ncol = 2, byrow = TRUE))


expand_grid2 <- function(...) {
  inputs.list <- list(...)
  inputs.list <- list(v1 = c(1, 2), 
                      v2 = c("a", "b", "c"), 
                      v3 = data.table(v3 = c("3a", "4a"), v4 = c("3b", "4b")))
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
  df <- as.data.table(df)
  
  
  df[, (c("v3", "v4")) := df$v3]
  
  df[, (c("v3", "v4")) := list(1: 12, 1: 12)]
  
  a <- df$v3
  setattr(df$v3, "class", "data.table")
  setattr(a, "class", "data.table")
  
  df2 <- setattr(df, "class", "data.table")
  df2[, (names(input.lists)[3]) := df2[, 3]]
  split(df2, by = "v3")
  
  df3 <- setattr(df, )
  return(df)
}