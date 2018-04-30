#' Plot Fitted Lognormal and Gamma Regression Models for Y vs. X
#'
#' User can specify parameters under which to generate X and Y, or specify X and
#' Y directly (e.g. if fitting models to real data).
#'
#' If \code{x} and \code{y} are not specified, data are generated as lognormal
#' or Gamma according to the following models. If \code{b} is not specified,
#' data are generated:
#'
#' X ~ N(0, 1)
#' Y|X ~ Lognormal(beta_0 + beta_x X, sigsq)
#'
#' If \code{b} is specified, data are generated:
#'
#' X ~ N(0, 1)
#' Y|X ~ Gamma(exp(beta_0 + beta_x X), b)
#'
#' Both models are fit using maximum likelihood, and the data are plotted with
#' the fitted curves for each model.
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param n Numeric value.
#' @param beta_0 Numeric value.
#' @param beta_c Numeric value.
#' @param sigsq Numeric value.
#' @param b Numeric value
#'
#'
#' @return Plot of E(X) vs. X according to each fitted model.
#'
#'
#'@export
plot_lognormal_gamma <- function(x = NULL, y = NULL,
                                 n = 1000,
                                 beta_0 = 0,
                                 beta_x = 0.5,
                                 sigsq = 0.1,
                                 b = NULL) {

  # Generate data if necessary
  if (is.null(x) || is.null(y)) {
    x <- rnorm(n = n)
    if (is.null(b)) {
      y <- rlnorm(n = n, meanlog = beta_0 + beta_x * x, sdlog = sqrt(sigsq))
    } else {
      y <- rgamma(n = n, shape = exp(beta_0 + beta_x * x), scale = b)
    }
  }

  # Fit models
  return(y)

}
