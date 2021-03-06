% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_2t_unequal.R
\name{n_2t_unequal}
\alias{n_2t_unequal}
\title{Calculate Per-Group Sample Size for Two-Sample Unequal Variance T-Test}
\usage{
n_2t_unequal(d, sigsq1, sigsq2, alpha = 0.05, beta = 0.2)
}
\arguments{
\item{d}{Numeric value specifying true difference in group means.}

\item{sigsq1, sigsq2}{Numeric value specifying the variance of observations in each group.}

\item{alpha}{Numeric value specifying type-1 error rate.}

\item{beta}{Numeric value specifying type-2 error rate.}
}
\value{
Numeric value indicating per-group sample size, rounded up to the
nearest whole number.
}
\description{
Unequal variance version of \code{\link{n_2t_equal}}. Assumes an equal sample
size for both groups, which is actually not optimal.
}
\examples{
# Per-group sample size for 90\% power to detect difference of 0.2 with
# sigsq's of 1 and 1.25
n_2t_unequal(d = 0.2, sigsq1 = 1, sigsq2 = 1.25, beta = 0.1)


}
