% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/which_min2.R
\name{which.min2}
\alias{which.min2}
\title{Return Index of (First) Minimum of a Vector}
\usage{
which.min2(x, arr.ind = FALSE, integer = FALSE)
}
\arguments{
\item{x}{Integer or numeric vector/matrix.}

\item{arr.ind}{Logical value for whether to return (row, col) position rather 
than vector position, if \code{x} is a matrix.}

\item{integer}{Logical value for whether \code{x} is an integer vector/matrix.}
}
\value{
Numeric value.
}
\description{
Returns index of minimum for vectors and index or (row, column) position for 
matrices. For optimal speed, use \code{integer = TRUE} if \code{x} is an 
integer vector/matrix and \code{integer = FALSE} otherwise. Typically faster 
than \code{\link[base]{which.min}} for matrices and for large vectors.
}
\examples{
# which.min2 vs. which.min for integer vector
x <- rpois(10000, lambda = 10)
all.equal(which.min(x), which.min2(x, integer = TRUE))
benchmark(which.min(x), which.min2(x, integer = TRUE), replications = 10000)

# which.min2 vs. which.min for numeric vector
x <- rnorm(10000)
all.equal(which.min(x), which.min2(x))
benchmark(which.min(x), which.min2(x), replications = 10000)


}
