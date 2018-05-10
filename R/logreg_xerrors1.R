#' Logistic Regression with Normal Exposure Subject to Additive Normal Errors
#'
#' Assumes exposure measurements are subject to additive normal measurement
#' error, and exposure given covariates is a normal-errors linear regression.
#' Some replicates are required for identifiability.
#'
#'
#' @param y Numeric vector of \code{Y} values.
#' @param xtilde List of numeric vectors with \code{Xtilde} values.
#' @param c Numeric matrix with \strong{\code{C}} values (if any), with
#' one row for each pool. Can be a vector if there is only 1 covariate.
#' @param prev Numeric value specifying disease prevalence, allowing for valid
#' estimation of the intercept with case-control sampling. Can specify
#' \code{samp_y1y0} instead if sampling rates are known.
#' @param samp_y1y0 Numeric vector of length 2 specifying sampling probabilities
#' for cases and controls, allowing for valid estimation of the intercept with
#' case-control sampling. Can specify \code{prev} instead if it's easier.
#' @param merror Logical value for whether there is measurement error.
#' @param approx_integral Logical value for whether to use the probit
#' approximation for the logistic-normal integral, to avoid numerically
#' integrating \code{X}'s out of the likelihood function.
#' @param integrate_tol Numeric value specifying the \code{tol} input to
#' \code{\link{hcubature}}. Only used if \code{approx_integral = FALSE}.
#' @param integrate_tol_hessian Same as \code{integrate_tol}, but for use when
#' estimating the Hessian matrix only. Sometimes more precise integration
#' (i.e. smaller tolerance) than used for maximizing the likelihood helps
#' prevent cases where the inverse Hessian is not positive definite.
#' @param estimate_var Logical value for whether to return variance-covariance
#' matrix for parameter estimates.
#' @param ... Additional arguments to pass to \code{\link[stats]{nlminb}}.
#'
#'
#' @return List containing:
#' \enumerate{
#' \item Numeric vector of parameter estimates.
#' \item Variance-covariance matrix (if \code{estimate_var = TRUE}).
#' \item Returned \code{\link[stats]{nlminb}} object from maximizing the
#' log-likelihood function.
#' \item Akaike information criterion (AIC).
#' }
#'
#'
#' @examples
#' # Load dataset - dat1 has (Y, C) values and dat1_xtilde is list with 1 or 2
#' # Xtilde measurements for each subject.
#' data(dat1)
#' data(dat1_xtilde)
#'
#' # Estimate log-OR for X and Y adjusted for C, ignoring measurement error
#' fit1 <- logreg_xerrors1(y = dat1$y, xtilde = dat1_xtilde, c = dat1$c,
#'                         merror = FALSE)
#' fit1$theta.hat
#'
#' # Repeat, but accounting for measurement error. Closer to true log-OR of 0.5.
#' fit2 <- logreg_xerrors1(y = dat1$y, xtilde = dat1_xtilde, c = dat1$c,
#'                         merror = TRUE)
#' fit2$theta.hat
#'
#'
#' @export
# betas <- c(0.5, 0.25, 0.1)
# alphas <- c(0.2, 0.1)
# sigsq_x.c <- 0.5
# sigsq_m <- 0.2
#
# n <- 100
# k <- rep(1, n)
# k[1: 10] <- 2
# c <- rnorm(n)
# x <- alphas[1] + alphas[2] * c + rnorm(n = n, sd = sqrt(sigsq_x.c))
# y <- rbinom(n, size = 1, prob = (1 + exp(-betas[1] - betas[2] * x - betas[3] * c))^(-1))
# xtilde <- list()
# for (ii in 1: n) {
#   xtilde[[ii]] <- x[ii] + rnorm(k[ii], sd = sqrt(sigsq_m))
# }
#
# truth <- logreg_xerrors1(y = y,
#                          xtilde = x,
#                          c = c,
#                          merror = FALSE,
#                          control = list(trace = 1))
# naive <- logreg_xerrors1(y = y,
#                          xtilde = sapply(xtilde, function(x) x[1]),
#                          c = c,
#                          merror = FALSE,
#                          control = list(trace = 1))
# corrected.noreps <- logreg_xerrors1(y = y,
#                                     xtilde = sapply(xtilde, function(x) x[1]),
#                                     c = c,
#                                     merror = TRUE,
#                                     control = list(trace = 1))
# corrected.reps <- logreg_xerrors1(y = y,
#                                   xtilde = xtilde,
#                                   c = c,
#                                   merror = TRUE,
#                                   control = list(trace = 1))
# corrected.reps <- logreg_xerrors1(y = y,
#                                   xtilde = xtilde,
#                                   c = c,
#                                   merror = TRUE,
#                                   approx_integral = FALSE,
#                                   control = list(trace = 1))
logreg_xerrors1 <- function(y,
                            xtilde,
                            c = NULL,
                            prev = NULL,
                            samp_y1y0 = NULL,
                            merror = TRUE,
                            approx_integral = TRUE,
                            integrate_tol = 1e-8,
                            integrate_tol_hessian = integrate_tol,
                            estimate_var = FALSE,
                            ...) {

  # Get name of xtilde input
  x.varname <- deparse(substitute(xtilde))
  if (length(grep("$", x.varname, fixed = TRUE)) > 0) {
    x.varname <- substr(x.varname,
                        start = which(unlist(strsplit(x.varname, "")) == "$") + 1,
                        stop = nchar(x.varname))
  }

  # Get information about covariates C
  if (is.null(c)) {
    c.varnames <- NULL
    n.cvars <- 0
    some.cs <- FALSE
  } else {
    c.varname <- deparse(substitute(c))
    if (class(c) != "matrix") {
      c <- as.matrix(c)
    }
    n.cvars <- ncol(c)
    some.cs <- TRUE
    c.varnames <- colnames(c)
    if (is.null(c.varnames)) {
      if (n.cvars == 1) {
        if (length(grep("$", c.varname, fixed = TRUE)) > 0) {
          c.varname <- substr(c.varname,
                              start = which(unlist(strsplit(c.varname, "")) == "$") + 1,
                              stop = nchar(c.varname))
        }
        c.varnames <- c.varname
      } else {
        c.varnames <- paste("c", 1: n.cvars, sep = "")
      }
    }
  }

  # Get number of betas and alphas
  n.betas <- 2 + n.cvars
  n.alphas <- 1 + n.cvars

  # Sample sizes
  n <- length(y)
  locs.cases <- which(y == 1)
  locs.controls <- which(y == 0)
  n1 <- length(locs.cases)
  n0 <- length(locs.controls)

  # Calculate offsets if prev or samp_y1y0 specified
  if (! is.null(prev)) {
    q <- rep(log(n1 / n0 * prev / (1 - prev)), n)
  } else if (! is.null(samp_y1y0)) {
    q <- rep(log(samp_y1y0[1] / samp_y1y0[2]), n)
  } else {
    q <- rep(0, n)
  }

  # If no measurement error and xtilde is a list, just use first measurements
  if (! merror & class(xtilde) == "list") {
    xtilde <- sapply(xtilde, function(x) x[1])
  }

  # Separate observations into precise Y, single imprecise Y, and replicate
  # imprecise Y's
  if (! merror) {
    some.p <- TRUE

    y.p <- y
    x.p <- xtilde
    onec.p <- cbind(rep(1, n), c)
    onexc.p <- cbind(rep(1, n), xtilde, c)
    q.p <- q

    some.s <- FALSE
    some.r <- FALSE

  } else {

    some.p <- FALSE
    if (class(xtilde) == "list") {
      k <- sapply(xtilde, length)
    } else {
      k <- rep(1, n)
    }

    which.s <- which(k == 1)
    n.s <- length(which.s)
    some.s <- n.s > 0
    if (some.s) {
      y.s <- y[which.s]
      xtilde.s <- unlist(xtilde[which.s])
      onec.s <- cbind(rep(1, n.s), c[which.s, , drop = FALSE])
      q.s <- q[which.s]
    }

    which.r <- which(k > 1)
    n.r <- length(which.r)
    some.r <- n.r > 0
    if (some.r) {
      k.r <- k[which.r]
      y.r <- y[which.r]
      xtilde.r <- xtilde[which.r]
      onec.r <- cbind(rep(1, n.r), c[which.r, , drop = FALSE])
      q.r <- q[which.r]
    }

  }

  # Get indices for parameters being estimated and create labels
  loc.betas <- 1: n.betas
  beta.labels <- paste("beta", c("0", x.varname, c.varnames), sep = "_")

  loc.alphas <- (n.betas + 1): (n.betas + n.alphas)
  alpha.labels <- paste("alpha", c("0", c.varnames), sep = "_")

  loc.sigsq_x.c <- n.betas + n.alphas + 1
  loc.sigsq_m <- n.betas + n.alphas + 2

  if (merror) {
    theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c", "sigsq_m")
  } else{
    theta.labels <- c(beta.labels, alpha.labels, "sigsq_x.c")
  }

  # Log-likelihood function for approximate ML
  llf.approx <- function(k,
                         y,
                         xtilde,
                         mu_x.c,
                         sigsq_x.c,
                         q,
                         beta_x,
                         cterm,
                         sigsq_m) {

    # E(X|Xtilde,C) and V(X|Xtilde,C)
    Mu_xxtilde.c <- matrix(mu_x.c, nrow = k + 1)
    Sigma_xxtilde.c_11 <- sigsq_x.c
    Sigma_xxtilde.c_12 <- matrix(sigsq_x.c, ncol = k)
    Sigma_xxtilde.c_21 <- t(Sigma_xxtilde.c_12)
    Sigma_xxtilde.c_22 <- sigsq_x.c + sigsq_m * diag(k)

    mu_x.xtildec <- Mu_xxtilde.c[1] + Sigma_xxtilde.c_12 %*%
      solve(Sigma_xxtilde.c_22) %*% (xtilde - Mu_xxtilde.c[-1])
    sigsq_x.xtildec <- Sigma_xxtilde.c_11 - Sigma_xxtilde.c_12 %*%
      solve(Sigma_xxtilde.c_22) %*% Sigma_xxtilde.c_21

    # Approximation of \int_X f(Y|X,C) f(X|Xtilde,C) dX
    t <- (q + beta_x * mu_x.xtildec + cterm) /
      sqrt(1 + sigsq_x.xtildec * beta_x^2 / 1.7^2)
    p <- exp(t) / (1 + exp(t))
    part1 <- dbinom(x = y, size = 1, prob = p, log = TRUE)

    # log[f(Xtilde|C)]
    part2 <- dmvnorm(x = xtilde, log = TRUE,
                     mean = Mu_xxtilde.c[-1],
                     sigma = Sigma_xxtilde.c_22)

    return(part1 + part2)

  }

  # Likelihood function for full ML
  lf.full <- function(k,
                      y,
                      xtilde,
                      x,
                      mu_x.c,
                      sigsq_x.c,
                      q,
                      beta_x,
                      cterm,
                      sigsq_m) {

    x <- matrix(x, nrow = 1)
    dens <- apply(x, 2, function(z) {

      # Transformation
      s <- z / (1 - z^2)

      # P(Y|X,C)
      p_y.xc <- (1 + exp(-q - beta_x * s - cterm))^(-1)

      # f(Y,X,Xtilde|C) = f(Y|X,C) f(Xtilde1|Xtilde2,X) f(Xtilde2|X) f(X|C)
      dbinom(x = y, size = 1, prob = p_y.xc) *
        prod(dnorm(x = xtilde, mean = s, sd = sqrt(sigsq_m))) *
        dnorm(x = s, mean = mu_x.c, sd = sqrt(sigsq_x.c))

    })

    # Back-transformation
    out <- matrix(dens * (1 + x^2) / (1 - x^2)^2, ncol = ncol(x))
    return(out)

  }

  # Log-likelihood function
  llf <- function(f.theta, estimating.hessian = FALSE) {

    # Extract parameters
    f.betas <- matrix(f.theta[loc.betas], ncol = 1)
    f.beta_0 <- f.betas[1]
    f.beta_x <- f.betas[2]
    f.beta_c <- matrix(f.betas[-c(1: 2)], ncol = 1)

    f.alphas <- matrix(f.theta[loc.alphas], ncol = 1)
    f.alpha_0 <- f.alphas[1]
    f.alpha_c <- matrix(f.alphas[-1], ncol = 1)

    f.sigsq_x.c <- f.theta[loc.sigsq_x.c]

    if (merror) {
      f.sigsq_m <- f.theta[loc.sigsq_x.c + 1]
    } else {
      f.sigsq_m <- 0
    }

    if (some.p) {

      # Log-likelihood for precise Y
      ll.p <- sum(
        dbinom(y.p, log = TRUE,
               size = 1, prob = (1 + exp(-q.p - onexc.p %*% f.betas))^(-1)) +
          dnorm(x.p, log = TRUE,
                mean = onec.p %*% f.alphas,
                sd = sqrt(f.sigsq_x.c))
      )

    } else {
      ll.p <- 0
    }

    # Set skip.rest flag to FALSE
    skip.rest <- FALSE

    if (some.s) {

      # Log-likelihood for single imprecise Xtilde

      if (approx_integral) {

        # Probit approximation for logistic-normal integral

        # E(X|Xtilde,C) and V(X|Xtilde,C)
        mu_x.c <- onec.s %*% f.alphas
        mu_x.xtildec <- mu_x.c + f.sigsq_x.c / (f.sigsq_x.c + f.sigsq_m) *
          (xtilde.s - mu_x.c)
        sigsq_x.xtildec <- f.sigsq_x.c - f.sigsq_x.c^2 / (f.sigsq_x.c + f.sigsq_m)

        t <- (q.s + onec.s %*% f.betas[-2] + f.beta_x * mu_x.xtildec) /
          sqrt(1 + sigsq_x.xtildec * f.beta_x^2 / 1.7^2)
        p <- exp(t) / (1 + exp(t))
        part1 <- sum(dbinom(x = y.s, size = 1, prob = p, log = TRUE))

        # log[f(Xtilde|C)]
        part2 <- sum(dnorm(x = xtilde.s, log = TRUE,
                           mean = mu_x.c, sd = sqrt(f.sigsq_x.c + f.sigsq_m)))

        ll.s <- part1 + part2

      } else {

        # Get integration tolerance
        if (estimating.hessian) {
          int.tol <- integrate_tol_hessian
        } else {
          int.tol <- integrate_tol
        }

        mu_x.c <- onec.s %*% f.alphas
        cterms <- onec.s %*% f.betas[-2]

        int.vals <- c()
        for (ii in 1: n.s) {

          # Perform integration
          int.ii <- cubature::hcubature(f = lf.full,
                                        tol = integrate_tol,
                                        lowerLimit = -1,
                                        upperLimit = 1,
                                        vectorInterface = TRUE,
                                        k = 1,
                                        y = y.s[ii],
                                        xtilde = xtilde.s[ii],
                                        mu_x.c = mu_x.c[ii],
                                        sigsq_x.c = f.sigsq_x.c,
                                        q = q.s[ii],
                                        beta_x = f.beta_x,
                                        cterm = cterms[ii],
                                        sigsq_m = f.sigsq_m)
          int.vals[ii] <- int.ii$integral
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            skip.rest <- TRUE
            break
          }

        }

        ll.s <- sum(log(int.vals))

      }

    } else {
      ll.s <- 0
    }

    if (some.r & ! skip.rest) {

      # Log-likelihood for replicate Y

      mu_x.c <- onec.r %*% f.alphas
      cterms <- onec.r %*% f.betas[-2]

      if (approx_integral) {

        ll.r <- sum(mapply(llf.approx,
                           k = k.r,
                           y = y.r,
                           xtilde = xtilde.r,
                           mu_x.c = mu_x.c,
                           sigsq_x.c = f.sigsq_x.c,
                           q = q.r,
                           beta_x = f.beta_x,
                           cterm = cterms,
                           sigsq_m = f.sigsq_m))

      } else {

        int.vals <- c()
        for (ii in 1: n.r) {

          # Perform integration
          int.ii <- cubature::hcubature(f = lf.full,
                                        tol = integrate_tol,
                                        lowerLimit = -1,
                                        upperLimit = 1,
                                        vectorInterface = TRUE,
                                        k = k.r[ii],
                                        y = y.r[ii],
                                        xtilde = unlist(xtilde.r[ii]),
                                        mu_x.c = mu_x.c[ii],
                                        sigsq_x.c = f.sigsq_x.c,
                                        q = q.r[ii],
                                        beta_x = f.beta_x,
                                        cterm = cterms[ii],
                                        sigsq_m = f.sigsq_m)
          int.vals[ii] <- int.ii$integral
          if (int.ii$integral == 0) {
            print(paste("Integral is 0 for ii = ", ii, sep = ""))
            print(f.theta)
            skip.rest <- TRUE
            break
          }

        }

        ll.r <- sum(log(int.vals))

      }

    } else {
      ll.r <- 0
    }

    # Return negative log-likelihood
    ll <- ll.p + ll.s + ll.r
    return(-ll)

  }

  # Create list of extra arguments, and assign default starting values and
  # lower values if not specified by user
  extra.args <- list(...)
  if (is.null(extra.args$start)) {
    if (merror) {
      extra.args$start <- c(rep(0.01, n.betas + n.alphas), 1, 1)
    } else {
      extra.args$start <- c(rep(0.01, n.betas + n.alphas), 1)
    }
  }
  if (is.null(extra.args$lower)) {
    if (merror) {
      extra.args$lower <- c(rep(-Inf, n.betas + n.alphas), 1e-3, 1e-3)
    } else {
      extra.args$lower <- c(rep(-Inf, n.betas + n.alphas), 1e-3)
    }
  }
  if (is.null(extra.args$control$rel.tol)) {
    extra.args$control$rel.tol <- 1e-6
  }
  if (is.null(extra.args$control$eval.max)) {
    extra.args$control$eval.max <- 1000
  }
  if (is.null(extra.args$control$iter.max)) {
    extra.args$control$iter.max <- 750
  }

  # Obtain ML estimates
  ml.max <- do.call(nlminb, c(list(objective = llf), extra.args))

  # Create list to return
  theta.hat <- ml.max$par
  names(theta.hat) <- theta.labels
  ret.list <- list(theta.hat = theta.hat)

  # If requested, add variance-covariance matrix to ret.list
  if (estimate_var) {
    hessian.mat <- pracma::hessian(f = llf, estimating.hessian = TRUE,
                                   x0 = theta.hat)
    theta.variance <- try(solve(hessian.mat), silent = TRUE)
    if (class(theta.variance) == "try-error") {
      print(hessian.mat)
      message("Estimated Hessian matrix is singular, so variance-covariance matrix cannot be obtained.")
      ret.list$theta.var <- NULL
    } else {
      colnames(theta.variance) <- rownames(theta.variance) <- theta.labels
      ret.list$theta.var <- theta.variance
    }
  }

  # Add nlminb object and AIC to ret.list
  ret.list$nlminb.object <- ml.max
  ret.list$aic <- 2 * (length(theta.hat) + ml.max$objective)

  # Return ret.list
  return(ret.list)

}


# # Prospective sampling
# n <- 10000
# n.reps <- n / 10
# betas <- c(-2, 0.5)
# alphas <- 0
# sigsq_x.c <- 1
# sigsq_m <- 0.25
#
# prev <- samp_y1y0 <- NULL
# merror <- TRUE
#
# x <- rnorm(n, mean = alphas, sd = sqrt(sigsq_x.c))
# y <- rbinom(n = n, size = 1, prob = (1 + exp(-betas[1] - betas[2] * x))^(-1))
#
# xtilde <- list()
# locs.2 <- sample(1: n, n.reps, replace = FALSE)
# for (ii in 1: length(x)) {
#   xtilde[[ii]] <- x[ii] + rnorm(n = ifelse(ii %in% locs.rep, 2, 1))
# }
#
# abc <- logreg_xerrors(y = y, xtilde = xtilde, control = list(trace = 1))
#
# # Try to get X very different in cases and controls
# n <- 1000000
# betas <- c(-10, 5)
# alphas <- 0
# sigsq_x.c <- 1
# x <- rnorm(n, mean = alphas, sd = sqrt(sigsq_x.c))
# y <- rbinom(n = n, size = 1, prob = (1 + exp(-betas[1] - betas[2] * x))^(-1))
# mean(y)
# par(mfrow = c(3, 1))
# histo(x[y == 1], xlim = c(-4, 4), breaks = 25, dis = "norm")
# histo(x[y == 0], xlim = c(-4, 4), breaks = 25, dis = "norm")
# histo(c(x[which(y == 1)[1: 1000]], x[which(y == 0)[1: 1000]]), breaks = 25, dis = "norm")
#
# # Case-control sampling
# n.pop <- 5000000
# n.samp <- 50000
# n.reps <- n.samp / 20
# betas <- c(-10, 5)
# alphas <- 0
# sigsq_x.c <- 1
# sigsq_m <- 0.1
#
# x <- rnorm(n = n.pop, mean = alphas, sd = sqrt(sigsq_x.c))
# y <- rbinom(n = n.pop, size = 1, prob = (1 + exp(-betas[1] - betas[2] * x))^(-1))
#
# loc.cases <- which(y == 1)
# loc.controls <- which(y == 0)
# loc.sampled <- c(sample(loc.cases, n.samp, replace = FALSE), sample(loc.controls, n.samp, replace = FALSE))
# x <- x[loc.sampled]
# y <- y[loc.sampled]
#
# xtilde <- list()
# locs.2 <- sample(1: (n.samp * 2), n.reps * 2, replace = FALSE)
# for (ii in 1: length(x)) {
#   xtilde[[ii]] <- x[ii] + rnorm(n = ifelse(ii %in% locs.2, 2, 1))
# }
#
# summary(glm(y ~ x, family = "binomial"))
# summary(glm(y ~ x, offset = rep(log(0.97/0.03), length(x)), family = "binomial"))
# summary(glm(y ~ sapply(xtilde, function(x) x[1]), offset = rep(log(0.97/0.03), length(x)), family = "binomial"))
# abc <- logreg_xerrors(y = y, xtilde = xtilde, prev = 0.03, control = list(trace = 1))
#
# x1 <- sapply(xtilde, function(x) x[1])
# x2 <- sapply(xtilde, function(x) x[2])
# plot(x1, x2)
#
# # Back to prospective sampling for this new scenario
# n <- 10000
# n.reps <- n / 10
# betas <- c(-10, 5)
# alphas <- 0
# sigsq_x.c <- 1
# sigsq_m <- 0.1
#
# x <- rnorm(n, mean = alphas, sd = sqrt(sigsq_x.c))
# y <- rbinom(n = n, size = 1, prob = (1 + exp(-betas[1] - betas[2] * x))^(-1))
#
# xtilde <- list()
# locs.2 <- sample(1: n, n.reps, replace = FALSE)
# for (ii in 1: length(x)) {
#   xtilde[[ii]] <- x[ii] + rnorm(n = ifelse(ii %in% locs.rep, 2, 1))
# }
#
# abc <- logreg_xerrors(y = y, xtilde = xtilde, control = list(trace = 1))
#
# # Loop through sampling rates and record estimates for single large-n trial
# betas <- c(-10, 5)
# alphas <- 0
# sigsq_x.c <- 1
# sigsq_m <- 0.1
#
# n.pop <- 5000000
# n.samp <- 100000
# n.samp <- 100000
# n.reps <- n.samp / 50
# n.reps <- 500
# vals <- c(seq(0.01, 0.1, 0.01), seq(0.2, 0.9, 0.1))
#
# estimates <- matrix(NA, nrow = length(vals), ncol = 5)
# for (ii in 1: length(vals)) {
#
#   p_case <- vals[ii]
#
#   x <- rnorm(n = n.pop, mean = alphas, sd = sqrt(sigsq_x.c))
#   y <- rbinom(n = n.pop, size = 1, prob = (1 + exp(-betas[1] - betas[2] * x))^(-1))
#
#   loc.cases <- which(y == 1)
#   loc.controls <- which(y == 0)
#   loc.sampled <- c(sample(loc.cases, n.samp * p_case, replace = FALSE),
#                    sample(loc.controls, n.samp * (1 - p_case), replace = FALSE))
#   x <- x[loc.sampled]
#   y <- y[loc.sampled]
#
#   xtilde <- list()
#   locs.2 <- sample(1: n.samp, n.reps, replace = FALSE)
#   for (jj in 1: length(x)) {
#     xtilde[[jj]] <- x[jj] + rnorm(n = ifelse(jj %in% locs.2, 2, 1), sd = sqrt(sigsq_m))
#   }
#
#   #summary(glm(y ~ x, family = "binomial"))
#   #summary(glm(y ~ x, offset = rep(log(0.97/0.03), length(x)), family = "binomial"))
#   #summary(glm(y ~ sapply(xtilde, function(x) x[1]), offset = rep(log(0.97/0.03), length(x)), family = "binomial"))
#   abc <- logreg_xerrors(y = y, xtilde = xtilde, prev = 0.03, control = list(trace = 1))
#   estimates[ii, ] <- abc$theta.hat
#
# }
