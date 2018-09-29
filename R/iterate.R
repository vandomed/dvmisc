#' Iterate Function Over All Combinations of User-Specified Inputs, Potentially 
#' Multiple Times
#' 
#' Basically a wrapper for \code{\link[purrr:map2]{pmap}}, but with some 
#' different functionality. It runs all combinations of inputs rather than the 
#' 1st set, 2nd set, and so forth, and multiple trials can be run for each 
#' scenario, which might be useful for running simulations.
#' 
#' @param f A function.
#' @param ... Arguments to \code{f}, any of which can be vector-valued.
#' @param fix List of non-scalar arguments to hold fixed rather than loop over 
#' (scalars can be passed through \code{...}).
#' @param trials Numeric value.
#' 
#' @return Data frame.
#' 
#' @examples
#' # Define function to generate data from N(mu, sigsq) and perform t-test.
#' f <- function(n = 100, mu = 0, sigsq = 1, alpha = 0.05) {
#'   x <- rnorm(n = n, mean = mu, sd = sqrt(sigsq))
#'   fit <- t.test(x = x, alpha = alpha)
#'   return(list(t = fit$statistic, p = fit$p.value))
#' }
#' 
#' # Call f once for various sample sizes and means
#' f %>% iterate(n = c(100, 500), mu = c(0.1, 0.25))
#' 
#' # Run 100 trials for each scenario and calculate empirical power
#' f %>% iterate(n = c(100, 500), mu = c(0.1, 0.25), trials = 100) %>%
#'   group_by(n, mu) %>%
#'   summarise(mean(p < 0.05))
#'
#' @export
iterate <- function(f, ..., fix = NULL, trials = 1) {
  
  # Construct data frame with inputs to give to pmap
  arg.combos <- expand_grid(..., stringsAsFactors = FALSE)
  
  # Loop through combinations and run however many trials of each set
  growing.list <- vector(mode = "list", length = nrow(arg.combos) * trials)
  index <- 0
  for (ii in 1: nrow(arg.combos)) {
    for (jj in 1: trials) {
      index <- index + 1
      growing.list[[index]] <- do.call(f, c(arg.combos[ii, ], fix))
    }
  }
  
  # Prep for merge depending on what f returns
  gl1 <- growing.list[[1]]
  if (is.list(gl1)) {
    if (is.null(names(gl1))) {
      n.each <- length(gl1)
      labels <- paste("V", 1: n.each, sep = "")
      growing.list <- lapply(growing.list, function(x) {
        y <- x
        names(y) <- labels
        return(y)
      })
    }
    premerge <- bind_rows(growing.list)
  } else {
    premerge <- do.call(rbind, growing.list)
  }
  
  # Return data table with results
  ret <- cbind(as.data.table(arg.combos[rep(1: nrow(arg.combos), each = trials), ]), 
               premerge)
  return(ret)
  
}
