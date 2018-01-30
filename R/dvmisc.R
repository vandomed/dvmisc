#' Faster Computation of Common Statistics and Miscellaneous Functions
#' 
#' Faster versions of base R functions (e.g. mean, standard deviation, 
#' covariance, weighted mean), mostly written in C++, along with miscellaneous 
#' functions for various purposes (e.g. create histogram with fitted probability 
#' density function or probability mass function curve, create body mass index 
#' groups, assess linearity assumption in logistic regression).
#' 
#' \tabular{ll}{
#' Package: \tab dvmisc \cr
#' Type: \tab Package \cr
#' Version: \tab 1.1.2 \cr
#' Date: \tab 2018-01-30 \cr
#' License: \tab GPL-2 \cr
#' }
#' 
#' The following functions are included: 
#' 
#' \code{\link{list.override}} \cr
#' \code{\link{cov_i}} \cr
#' \code{\link{inside}} \cr
#' \code{\link{interval.groups}} \cr
#' \code{\link{quant.groups}}
#' 
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#' 
#' @references Acknowledgment: This material is based upon work supported by the 
#' National Science Foundation Graduate Research Fellowship under Grant No. 
#' DGE-0940903.
#' 
#' @docType package
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @importFrom rbenchmark benchmark
#' @useDynLib dvmisc, .registration=TRUE
#' @name dvmisc
NULL