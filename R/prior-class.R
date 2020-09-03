#'
#' Class containing a prior distribution derived from life-history data
#' 
#' This is an S4 object class that includes both a numeric vector for storage of derived values generated using Monte Carlo methods, such as the intrinsic growth rate \eqn{r}, and a list of parameters describing the associated parameteric distribution. Currently only a log-normal distribution is supported.
#' 
#' If \code{length(x)>1} then the function creates an object containing values of \code{x}, otherwise it creates a vector of zero's of length equal to \code{x}. 
#'
#' For example, values for \eqn{r} can be simulated directly or generated using the \code{\link{rCalc}} function. The class contains an additional slot to hold parameters of the log-normal distribution, which is used by \pkg{bdm} to describe the prior for \eqn{r}.
#' 
#' @slot .Data numeric vector of derived values
#' @slot lognormal.par log-normal distribution parameter values
#'
#' @export
setClass("prior", contains = "numeric", slots = list(lognormal.par = "list", generation.time = "numeric"))
