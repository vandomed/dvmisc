#' Plot Points +/- Error Bars
#' 
#' Creates plot showing user-specified points (e.g. means, medians, regression 
#' coefficients) along with user-specified error bars (e.g. standard deviations, 
#' min/max, 95\% confidence intervals).
#' 
#' @param y Numeric vector of y-values for different groups, or numeric matrix 
#' where each column contains y-values for clustered subgroups within a group.
#' @param bars Numeric vector or matrix (matching whichever type \code{y} is) 
#' specifying the length of the error bar for each group/subgroup (i.e. distance 
#' from point to one end of error bar).
#' @param bars.lower Numeric vector or matrix (matching whichever type \code{y} 
#' is) specifying the position of the lower end of the error bar for each 
#' group/subgroup.
#' @param bars.upper Numeric vector or matrix (matching whichever type \code{y} 
#' is) specifying the position of the upper end of the error bar for each 
#' group/subgroup.
#' @param group.labels Character vector giving labels for the groups.
#' @param subgroup.labels Character vector giving labels for the subgroups.
#' @param subgroup.pch Plotting symbol for different subgroups within each 
#' group.
#' @param subgroup.col Plotting color for different subgroups within each group.
#' @param points.list Optional list of inputs to pass to 
#' \code{\link[graphics]{points}} function.
#' @param arrows.list Optional list of inputs to pass to 
#' \code{\link[graphics]{arrows}} function.
#' @param axis.list Optional list of inputs to pass to 
#' \code{\link[graphics]{axis}} function.
#' @param legend.list Optional list of inputs to pass to 
#' \code{\link[graphics]{legend}} function.
#' @param ... Additional arguments to pass to \code{\link[graphics]{plot}} 
#' function.
#' 
#' @return Plot showing points +/- error bars across groups/subgroups.
#' 
#' @examples
#' # Generate 100 values from normal distributions with different means, and 
#' # graph mean +/- standard deviation across groups 
#' dat <- cbind(rnorm(100, 2), rnorm(100, 2.5), rnorm(100, 1.75))
#' means <- apply(dat, 2, mean)
#' sds <- apply(dat, 2, sd)
#' fig1 <- dots_bars(y = means, bars = sds, main = "Mean +/- SD by Group",
#'                   ylab = "Mean +/- SD")
#'                   
#' # Simulate BMI values for males and females in 3 different age groups, and 
#' # graph mean +/- 95\% CI
#' sex <- as.factor(c(rep("Male", 300), rep("Female", 300)))
#' age <- as.factor(rep(c("Young", "Middle", "Old"), 2))
#' bmi <- c(rnorm(100, 25, 4), rnorm(100, 26, 4.25), rnorm(100, 27, 4.5),
#'          rnorm(100, 26.5, 4.5), rnorm(100, 27.25, 4.75), rnorm(100, 28, 5))
#' dat <- data.frame(sex = sex, age = age, bmi = bmi)
#' means <- tapply(dat$bmi, dat[, c("sex", "age")], mean)
#' ci.lower <- tapply(dat$bmi, dat[, c("sex", "age")],
#'                    function(x) t.test(x)$conf.int[1])
#' ci.upper <- tapply(dat$bmi, dat[, c("sex", "age")],
#'                    function(x) t.test(x)$conf.int[2])
#' fig2 <- dots_bars(y = means, bars.lower = ci.lower, bars.upper = ci.upper,
#'                   main = "BMI by Sex and Age",
#'                   ylab = "BMI (mean +/- CI)",
#'                   xlab = "Age group")
#'
#' @export
dots_bars <- function(y = NULL,
                      bars = NULL,
                      bars.lower = y - bars,
                      bars.upper = y + bars,
                      group.labels = NULL,
                      subgroup.labels = NULL,
                      subgroup.pch = NULL,
                      subgroup.col = NULL,
                      points.list = NULL,
                      arrows.list = NULL,
                      axis.list = NULL,
                      legend.list = NULL,
                      ...) {
  
  if (! is.matrix(y) | (is.matrix(y) && (ncol(y) == 1 | nrow(y) == 1))) {
    
    # Code to execute if there are no subgroups
    
    # Create x-values for plot
    xvals <- 1: length(y)
    
    # If NULL, assign generic values to group.labels
    if (is.null(group.labels)) {
      if (is.null(names(y))) {
        group.labels <- xvals
      } else {
        group.labels <- names(y)
      }
    }
    
    # Create list of extra arguments
    extra.args <- list(...)
    
    # If NULL, figure out default values for various plot features
    if (is.null(extra.args$ylab)) {
      extra.args$ylab = deparse(substitute(y))
    }
    if (is.null(extra.args$xlab)) {
      extra.args$xlab = "Group"
    }
    if (is.null(extra.args$main)) {
      extra.args$main <- paste(extra.args$ylab, " by ", extra.args$xlab, sep = "")
    }
    if (is.null(extra.args$ylim)) {
      yrange <- max(bars.upper) - min(bars.lower)
      extra.args$ylim <- c(min(bars.lower) - 0.05 * yrange,
                           max(bars.upper) + 0.05 * yrange)
      if (all(extra.args$ylim > 0)) {
        extra.args$ylim[1] <- 0
      } else if (all(extra.args$ylim < 0)) {
        extra.args$ylim[2] <- 0
      }
    }
    if (is.null(extra.args$xlim)) {
      extra.args$xlim <- c(min(xvals) - 0.75, max(xvals) + 0.75)
    }
    
    # Create plot
    do.call(plot, c(list(x = xvals, y = y, xaxt = "n", type = "n"),
                    extra.args))
    
    # Add points and error bars
    do.call(points, c(list(x = xvals, y = y), points.list))
    arrows.list <- list_override(list1 = list(length = 0.05, angle = 90,
                                              code = 3),
                                 list2 = arrows.list)
    do.call(arrows, c(list(x0 = xvals, y0 = bars.lower,
                           x1 = xvals, y1 = bars.upper),
                      arrows.list))
    
    # Add group labels on x-axis
    axis.list <- list_override(list1 = list(side = 1, at = xvals,
                                            labels = group.labels),
                               list2 = axis.list)
    do.call(axis, axis.list)
    
  } else {
    
    # Code to execute if there are subgroups
    
    # Get number of groups and number of subgroups within each group
    group.n <- ncol(y)
    subgroup.n <- nrow(y)
    
    # Create x-values for plot
    xvals <- 1: group.n
    
    # If NULL, assign generic values to group.labels, subgroup.labels, and
    # subgroup.pch
    if (is.null(group.labels)) {
      if (is.null(colnames(y))) {
        group.labels <- xvals
      } else {
        group.labels <- colnames(y)
      }
    }
    if (is.null(subgroup.labels)) {
      if (is.null(rownames(y))) {
        subgroup.labels <- LETTERS[1: subgroup.n]
      } else {
        subgroup.labels <- rownames(y)
      }
    }
    if (is.null(subgroup.pch)) {
      if (subgroup.n <= 5) {
        subgroup.pch <- c(1, 18, 8, 0, 4)[1: subgroup.n]
      } else {
        subgroup.pch <- 1: subgroup.n
      }
    }
    if (is.null(subgroup.col)) {
      subgroup.col <- rep("black", subgroup.n)
    }
    
    # Create list of extra arguments
    extra.args <- list(...)
    
    # If NULL, figure out default values for various plot features
    if (is.null(extra.args$ylab)) {
      extra.args$ylab = deparse(substitute(y))
    }
    if (is.null(extra.args$xlab)) {
      extra.args$xlab = "Group"
    }
    if (is.null(extra.args$main)) {
      extra.args$main <- paste(extra.args$ylab, " by ", extra.args$xlab, sep = "")
    }
    if (is.null(extra.args$ylim)) {
      yrange <- max(bars.upper) - min(bars.lower)
      extra.args$ylim <- c(min(bars.lower) - 0.05 * yrange,
                           max(bars.upper) + 0.05 * yrange)
      if (all(extra.args$ylim > 0)) {
        extra.args$ylim[1] <- 0
      } else if (all(extra.args$ylim < 0)) {
        extra.args$ylim[2] <- 0
      }
    }
    if (is.null(extra.args$xlim)) {
      extra.args$xlim <- c(min(xvals) - 0.75, max(xvals) + 0.75)
    }
    
    # Create plot
    do.call(plot, c(list(cbind(xvals, t(y)), xaxt = "n", type = "n"),
                    extra.args))
    
    # Create x.steps vector to offset subgroups
    x.steps <- seq(-0.15, 0.15, 0.3 / (subgroup.n - 1))
    
    # Loop through and add points and bars
    arrows.list <- list_override(list1 = list(length = 0.05, angle = 90,
                                              code = 3),
                                 list2 = arrows.list)
    for (ii in 1: subgroup.n) {
      do.call(points, c(list(x = xvals + x.steps[ii], y = y[ii, ],
                             pch = subgroup.pch[ii], col = subgroup.col[ii]),
                        points.list))
      do.call(arrows, c(list(x0 = xvals + x.steps[ii], y0 = bars.lower[ii, ],
                             x1 = xvals + x.steps[ii], y1 = bars.upper[ii, ]),
                        arrows.list))
    }
    
    # Add group labels on x-axis
    axis.list <- list_override(list1 = list(side = 1, at = xvals,
                                            labels = group.labels),
                               list2 = axis.list)
    do.call(axis, axis.list)
    
    # Add legend
    legend.list <- list_override(list1 = list(x = "bottomleft",
                                              pch = subgroup.pch,
                                              legend = subgroup.labels),
                                 list2 = legend.list)
    do.call(legend, legend.list)
    
  }
}