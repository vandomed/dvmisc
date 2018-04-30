#' Fit Constant-Scale Gamma Model for Y vs. Covariates
#'
#' Uses maximum likelihood to fit
#' Y|\strong{X} ~ Gamma(exp(beta_0 + \strong{beta_x}^T \strong{X}), b)
#'
#' @param y Numeric vector.
#' @param x Numeric vector or matrix. If \code{NULL}, model reduces to marginal
#' Gamma model with shape parameter exp(beta_0).
#' @param var Logical value for whether to return Hessian-based
#' variance-covariance matrix.
#'
#' @return List of parameter estimates, variance-covariance matrix (if
#' requested), and AIC.
#'
#'
#' @examples
#' # Generate data
#' set.seed(123)
#' x <- rnorm(1000)
#' y <- rgamma(1000, shape = exp(0.5 + 0.25 * x), scale = 0.25)
#'
#' # Fit model
#' fit <- gamma_constantscale(y = y, x = x)
#' fit$theta.hat
#' fit$varcov
#' fit$aic
#'
#' # Plot E(Y) vs. X according to model fit
#' plot(x, y, main = "Fitted Gamma Model for Y vs. X")
#' xvals <- seq(min(x), max(x), 0.01)
#' yvals <- exp(fit$theta.hat[1] + fit$theta.hat[2] * xvals) * fit$theta.hat[3]
#' points(xvals, yvals, type = "l")
#'
#'
#'@export
gamma_constantscale <- function(y, x = NULL, var = TRUE) {

  # Design matrix
  n <- length(y)
  onex <- cbind(rep(1, n), x)
  p <- ncol(onex)

  # Labels
  labs <- c(paste("x", 0: (p - 1), sep = ""), "b")

  # Log-likelihood function
  ll.f <- function(f.theta) {

    f.betas <- f.theta[1: p]
    f.b <- f.theta[p + 1]
    ll <- sum(dgamma(x = y, log = TRUE,
                     shape = exp(onex %*% f.betas),
                     scale = f.b))
    return(-ll)

  }

  # Maximize log-likelihood
  llmax <- nlminb(objective = ll.f,
                  start = c(rep(0, p), 1),
                  lower = c(rep(-Inf, p), 1e-6))
  theta.hat <- llmax$par
  names(theta.hat) <- labs
  ret.list <- list(theta.hat = theta.hat)

  # Variance-covariance matrix
  if (var) {
    hessian.mat <- pracma::hessian(f = ll.f, x0 = theta.hat)
    varcov <- solve(hessian.mat)
    colnames(varcov) <- rownames(varcov) <- labs
    ret.list$varcov <- varcov
  }

  # AIC
  ret.list$aic <- 2 * (p + 1 + llmax$objective)

  # Return ret.list
  return(ret.list)

}
