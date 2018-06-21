#' Logistic Regression with Lognormal Exposure Subject to Multiplicative 
#' Lognormal Errors
#'
#' Assumes exposure measurements are subject to multiplicative mean-1 lognormal 
#' measurement errors, and exposure given covariates is a log-transformed linear 
#' regression. Some replicates are required for identifiability. Parameters are 
#' estimated using maximum likelihood.
#' 
#' Disease model is:
#' 
#' logit[P(Y = 1|X, \strong{C})] = beta_0 + beta_x X + \strong{beta_c}^T 
#' \strong{C}
#' 
#' Measurement error model is:
#' 
#' Xtilde|X ~ LN(-1/2 sigsq_m, sigsq_m)
#' 
#' Exposure model is:
#' 
#' X|\strong{C} ~ Lognormal(alpha_0 + \strong{alpha_c}^T \strong{C}, sigsq_x.c)
#'
#'
#' @inheritParams logreg_xerrors1
#' 
#' 
#' @examples
#' # Load dataset - dat3 has (Y, X, C) values and dat3_xtilde is list with 1 or 
#' # 2 Xtilde measurements for each subject. True log-OR (beta_x) is 0.2.
#' data(dat3)
#' data(dat3_xtilde)
#' 
#' # Unobservable truth - use true X's
#' fit1 <- logreg_xerrors3(y = dat3$y, xtilde = dat3$x, c = dat3$c, 
#'                         merror = FALSE)
#' fit1$theta.hat
#' 
#' # Naive - use Xtilde's but ignore measurement error. Note that when merror is 
#' # set to FALSE, first Xtilde value for each subject is used.
#' fit2 <- logreg_xerrors3(y = dat3$y, xtilde = dat3_xtilde, c = dat3$c,
#'                         merror = FALSE)
#' fit2$theta.hat
#'
#' # Corrected - takes a few minutes due to numerical integration.
#' \dontrun{
#' fit3 <- logreg_xerrors3(y = dat3$y, xtilde = dat3_xtilde, c = dat3$c,
#'                         merror = TRUE, integrate_tol = 1e-4)
#' fit3$theta.hat
#' }
#' 
#' 
#' @export
logreg_xerrors3 <- function(y,
                            xtilde,
                            c = NULL,
                            prev = NULL,
                            samp_y1y0 = NULL,
                            merror = TRUE,
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
  
  
  # Likelihood function for full ML
  if (some.s | some.r) {
    lf <- function(y,
                   xtilde,
                   x,
                   meanlog, 
                   sdlog, 
                   q,
                   beta_x,
                   cterm,
                   sigsq_m) {
      
      x <- matrix(x, nrow = 1)
      dens <- apply(x, 2, function(z) {
        
        # Transformation
        s <- z / (1 - z)
        
        # P(Y|X,C)
        p_y.xc <- (1 + exp(-q - beta_x * s - cterm))^(-1)
        
        # f(Y,X,Xtilde|C) = f(Y|X,C) f(Xtilde|X) f(X|C)
        dbinom(y,
               size = 1,
               prob = p_y.xc) *
          prod(dlnorm(xtilde,
                      meanlog = log(s) - 1/2 * sigsq_m,
                      sdlog = sqrt(sigsq_m))) *
          dlnorm(s,
                 meanlog = meanlog,
                 sdlog = sdlog)
        
      })
      
      # Back-transformation
      out <- matrix(dens / (1 - x)^2, ncol = ncol(x))
      return(out)
      
    }
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
      
      # Log-likelihood for precise X
      ll.p <- sum(
        dbinom(y.p, log = TRUE,
               size = 1,
               prob = (1 + exp(-q.p - onexc.p %*% f.betas))^(-1)) +
          dlnorm(x.p, log = TRUE,
                 meanlog = onec.p %*% f.alphas,
                 sdlog = sqrt(f.sigsq_x.c))
      )
      
    } else {
      ll.p <- 0
    }
    
    # Set skip.rest flag to FALSE
    skip.rest <- FALSE
    
    if (some.s) {
      
      # Log-likelihood for single imprecise Xtilde
      
      # Get integration tolerance
      int.tol <- ifelse(estimating.hessian, integrate_tol_hessian, integrate_tol)
      
      meanlogs <- onec.s %*% f.alphas
      cterms <- onec.s %*% f.betas[-2]
      
      int.vals <- c()
      for (ii in 1: n.s) {
        
        # Perform integration
        int.ii <- cubature::hcubature(f = lf,
                                      tol = int.tol,
                                      lowerLimit = 0,
                                      upperLimit = 1,
                                      vectorInterface = TRUE,
                                      y = y.s[ii],
                                      xtilde = xtilde.s[ii],
                                      meanlog = meanlogs[ii],
                                      sdlog = sqrt(f.sigsq_x.c),
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
      
    } else {
      ll.s <- 0
    }
    
    if (some.r & ! skip.rest) {
      
      # Log-likelihood for replicate Y
      
      meanlogs <- onec.r %*% f.alphas
      cterms <- onec.r %*% f.betas[-2]
      
      int.vals <- c()
      for (ii in 1: n.r) {
        
        # Perform integration
        int.ii <- cubature::hcubature(f = lf,
                                      tol = integrate_tol,
                                      lowerLimit = 0,
                                      upperLimit = 1,
                                      vectorInterface = TRUE,
                                      y = y.r[ii],
                                      xtilde = unlist(xtilde.r[ii]),
                                      meanlog = meanlogs[ii], 
                                      sdlog = sqrt(f.sigsq_x.c), 
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
