#' Histogram with Added Options
#' 
#' Similar to base R function \code{\link[graphics]{hist}}, but with two added 
#' features: (1) Can overlay a fitted probability density/mass function 
#' (PDF/PMF) for any univariate distribution supported in R (see 
#' \code{\link[stats]{Distributions}}); and (2) Can generate more of a barplot 
#' type histogram, where each possible value gets its own bin centered over its 
#' value (useful for discrete variables with not too many possible values).
#' 
#' When \code{x} takes on whole numbers, you typically want to set 
#' \code{dis.shift = -0.5} if \code{right = TRUE} 
#' (\code{\link[graphics]{hist}}'s default) and \code{dis.shift = 0.5} if 
#' \code{right = FALSE}. The function will do this internally by default.
#' 
#' To illustrate, suppose a particular bin represents \code{(7, 10]}. Its 
#' midpoint will be at \code{x = 8.5} on the graph. But if input values are 
#' whole numbers, this bin really only includes values of 8, 9, and 10, which 
#' have a mean of 9. So you really want \code{f(9)} to appear at \code{x = 8.5}. 
#' This requires shifting the curve to the left 0.5 units, i.e. setting 
#' \code{dis.shift = -0.5}.
#' 
#' When \code{x} takes on whole numbers with not too many unique values, you may 
#' want the histogram to show one bin for each integer. You can do this by 
#' setting \code{integer.breaks = TRUE}. By default, the function sets 
#' \code{integer.breaks = TRUE} if \code{x} contains whole numbers with 10 or 
#' fewer unique values.
#' 
#' @param x Numeric vector of values.
#' @param dis Character vector indicating which distribution should be used to 
#' add fitted PDF/PMF to the histogram. Possible values are \code{"none"}, 
#' \code{"beta"}, \code{"binom"} (must specify \code{size}), \code{"cauchy"}, 
#' \code{"chisq"}, \code{"exp"}, \code{"f"}, \code{"gamma"}, \code{"geom"}, 
#' \code{"hyper"} (must specify total number of balls in urn, \code{N}, and 
#' number of balls drawn each time, \code{k}), \code{"lnorm"}, \code{"nbinom"} 
#' (must specify \code{size}), \code{"norm"}, \code{"pois"}, \code{"t"}, 
#' \code{"unif"}, and \code{"weibull"}.
#' @param dis.shift Numeric value for shifting the fitted PDF/PMF along the 
#' x-axis of the histogram.
#' @param integer.breaks If \code{TRUE}, integers covering the range of \code{x} 
#' are used for breaks, so there is one bin for each integer. Useful for 
#' discrete distributions that don't take on too many unique values.
#' @param points.list Optional list of inputs to pass to 
#' \code{\link[graphics]{points}} function, which is used to add the fitted 
#' PDF/PMF.
#' @param axis.list Optional list of inputs to pass to 
#' \code{\link[graphics]{axis}} function.
#' ... May include arguments to pass to \code{\link[graphics]{hist}} and/or 
#' parameter values needed for certain distributions (\code{size} if 
#' \code{dis = "binom"} or \code{dis = "nbinom"}, \code{N} and \code{k} if 
#' \code{dis = "hyper"}).
#' 
#' @return Histogram with fitted PDF/PMF if requested.
#' 
#' @examples
#' # Generate 10,000 Poisson(2) values. Compare default histograms from hist vs. 
#' # histo.
#' set.seed(123)
#' x <- rpois(n = 10000, lambda = 2)
#' par(mfrow = c(1, 2))
#' hist(x)
#' histo(x)
#' 
#' # Generate 10,000 lognormal(0, 0.35) values. Create histograms with curves
#' # showing fitted log-normal and normal PDFs.
#' set.seed(123)
#' x <- rlnorm(n = 10000, meanlog = 0, sdlog = 0.35)
#' par(mfrow = c(1, 2))
#' histo(x, "lnorm", main = "Log-normal curve")
#' histo(x, "norm", main = "Normal curve")
#' 
#' # Generate 10,000 Binomial(8, 0.25) values. Create histogram, specifying 
#' size = 5, with blue line/points showing fitted PMF.
#' set.seed(123)
#' x <- rbinom(n = 10000, size = 5, prob = 0.25)
#' par(mfrow = c(1, 1))
#' histo(x, "binom", size = 5, points.list = list(type = "b", col = "blue"))
#' 
#' @export
histo <- function(x,
                  dis = "none", dis.shift = NULL,
                  integer.breaks = NULL,
                  points.list = NULL,
                  axis.list = NULL,
                  ...) {
  
  # Create list with ... arguments
  extra.args <- list(...)
  
  # Extract any parameters (i.e. arguments NOT for hist) included in ...
  if (!is.null(extra.args)) {
    loc <- which(names(extra.args) == "size")
    if (length(loc) == 1) {
      size <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
    loc <- which(names(extra.args) == "N")
    if (length(loc) == 1) {
      N <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
    loc <- which(names(extra.args) == "k")
    if (length(loc) == 1) {
      k <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
  }
  
  # If integer.breaks is NULL, set to TRUE if x takes on whole numbers with 20
  # or fewer distinct values, else set to FALSE
  if (is.null(integer.breaks)) {
    integer.breaks <- all(x %% 1 == 0) & length(unique(x)) <= 10
  }
  
  # If right is not specified or integer.breaks is TRUE, set to TRUE
  if (! "right" %in% names(extra.args) | integer.breaks) {
    extra.args$right <- TRUE
  }
  
  # If integer.breaks is TRUE, make breaks a vector of integers
  # covering the range of x
  if (integer.breaks) {
    extra.args$breaks <- seq(floor(min(x)) - 1, ceiling(max(x)), 1)
  }
  
  # If freq is not specified, set to FALSE
  if (! "freq" %in% names(extra.args)) {
    extra.args$freq <- FALSE
  }
  
  # If xlab/main not specified, set
  if (! "xlab" %in% names(extra.args)) {
    extra.args$xlab <- deparse(substitute(x))
  }
  if (! "main" %in% names(extra.args)) {
    extra.args$main <- paste("Histogram of ", deparse(substitute(x)), sep = "")
  }
  
  # Create histogram
  if (integer.breaks) {
    hist.fig <- do.call(hist, c(list(x = quote(x), xaxt = "n"), extra.args))
    hist.fig <- do.call(axis, c(list(side = 1, at = hist.fig$mids,
                                     labels = hist.fig$breaks[-1]),
                                axis.list))
  } else {
    hist.fig <- do.call(hist, c(list(x = quote(x)), extra.args))
  }
  
  # Add fitted pdf/pmf if requested
  if (dis != "none") {
    
    if (dis == "beta") {
      
      theta.hat <- fitdistr(x, "beta", start = list(shape1 = 0.5,
                                                    shape2 = 0.5))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dbeta(x = x.vals, shape1 = theta.hat[1], shape2 = theta.hat[2])
      
    } else if (dis == "binom") {
      
      # Need user-input value for size
      p.hat <- mean(x) / size
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dbinom(x = x.vals, size = size, prob = p.hat)
      
    } else if (dis == "cauchy") {
      
      theta.hat <- fitdistr(x, "cauchy")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <-
        dcauchy(x = x.vals, location = theta.hat[1], scale = theta.hat[2])
      do.call(points, c(list(x = x.vals, y = y.vals, type = "l"), points.list))
      
    } else if (dis == "chisq") {
      
      theta.hat <- fitdistr(x, "chi-squared", start = list(df = 1))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dchisq(x = x.vals, df = theta.hat[1])
      
    } else if (dis == "exp") {
      
      theta.hat <- fitdistr(x, "exponential")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dexp(x = x.vals, rate = theta.hat)
      
    } else if (dis == "f") {
      
      theta.hat <- fitdistr(x, "f", start = list(df1 = 1, df2 = 2))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- df(x = x.vals, df1 = theta.hat[1], df2 = theta.hat[2])
      
    } else if (dis == "gamma") {
      
      theta.hat <- fitdistr(x, "gamma",
                            start = list(shape = 1, scale = 1))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dgamma(x = x.vals, shape = theta.hat[1], scale = theta.hat[2])
      do.call(points, c(list(x = x.vals, y = y.vals, type = "l"), points.list))
      
    } else if (dis == "geom") {
      
      theta.hat <- fitdistr(x, "geometric")$estimate
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dgeom(x = x.vals, prob = theta.hat)
      
    } else if (dis == "hyper") {
      
      # Need user-input values for N, k
      loglik.f.hyper <- function(m) {
        n <- N - m
        ll <- sum(log(choose(m, x) * choose(n, k - x) / choose(n + m, k)))
        return(-ll)
      }
      m.hat <- round(nlminb(objective = loglik.f.hyper, start = k)$par)
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dhyper(x = x.vals, m = m.hat, n = N - m.hat, k = k)
      
    } else if (dis == "lnorm") {
      
      theta.hat <- fitdistr(x, "lognormal")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dlnorm(x = x.vals, meanlog = theta.hat[1], sdlog = theta.hat[2])
      
    } else if (dis == "nbinom") {
      
      loglik.f.nbinom <- function(p) {
        ll <- sum(log(gamma(x + size) / (gamma(size) * factorial(x)) *
                        p^size * (1 - p)^x))
        return(-ll)
      }
      p.hat <- nlminb(objective = loglik.f.nbinom, start = 0.5)$par
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dnbinom(x = x.vals, size = size, prob = p.hat)
      
    } else if (dis == "norm") {
      
      theta.hat <- fitdistr(x, "normal")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dnorm(x = x.vals, mean = theta.hat[1], sd = theta.hat[2])
      
    } else if (dis == "pois") {
      
      theta.hat <- fitdistr(x, "poisson")$estimate
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dpois(x = x.vals, lambda = theta.hat)
      
    } else if (dis == "t") {
      
      theta.hat <- fitdistr(x, "t")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dt(x = x.vals, df = theta.hat[3])
      
    } else if (dis == "unif") {
      
      min.hat <- min(x)
      max.hat <- max(x)
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dunif(x = x.vals, min = min.hat, max = max.hat)
      
    } else if (dis == "weibull") {
      
      theta.hat <- fitdistr(x, "weibull")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dweibull(x = x.vals, shape = theta.hat[1], scale = theta.hat[2])
      
    }
    
    # If dis.shift is NULL and all values in x are integers, figure out how to
    # shift curve to make it match up with the histogram bars
    if (is.null(dis.shift)) {
      if (all(x %% 1 == 0)) {
        dis.shift <- ifelse(extra.args$right, -0.5, 0.5)
      } else {
        dis.shift <- 0
      }
    }
    
    # Add overlaying curve
    points.list <- list.override(list1 = list(type = "l"),
                                 list2 = points.list)
    do.call(points, c(list(x = x.vals + dis.shift, y = y.vals),
                      points.list))
    
  }
  
}