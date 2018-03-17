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
#' Version: \tab 1.1.3 \cr
#' Date: \tab 2018-03-17 \cr
#' License: \tab GPL-3 \cr
#' }
#' 
#' See \href{https://cran.r-project.org/package=dvmisc}{CRAN documentation} for 
#' full list of functions. 
#' 
#' @author Dane R. Van Domelen \cr \email{vandomed@@gmail.com}
#' 
#' @references 
#' Eddelbuettel, D. and Francois, R. (2011) Rcpp: Seamless R and C++ 
#' Integration. Journal of Statistical Software, 40(8), 1-18. 
#' \url{http://www.jstatsoft.org/v40/i08/}.
#' 
#' Eddelbuettel, D. (2013) Seamless R and C++ Integration with Rcpp. Springer, 
#' New York. ISBN 978-1-4614-6867-7.
#' 
#' Eddelbuettel, D. and Balamuta, J.J. (2017). Extending R with C++: A Brief 
#' Introduction to Rcpp. PeerJ Preprints 5:e3188v1. 
#' \url{https://doi.org/10.7287/peerj.preprints.3188v1}.
#' 
#' Acknowledgment: This material is based upon work supported by the 
#' National Science Foundation Graduate Research Fellowship under Grant No. 
#' DGE-0940903.
#' 
#' @docType package
#' @import graphics
#' @importFrom MASS fitdistr
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @importFrom rbenchmark benchmark
#' @import stats
#' @importFrom utils head tail
#' @useDynLib dvmisc, .registration=TRUE
#' @name dvmisc
NULL