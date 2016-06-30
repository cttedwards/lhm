#' Class containing life-history information
#' 
#' This is an S4 object class that contains the life-history information necessary to calculate derived quantities using life history theory, such as the intrinsic growth rate \eqn{r} using the Euler-Lotka equation. 
#'
#' Initialisation is usually through a call to \code{rdat(ainf,iter)}. Using this approach specific life-history data can then be included through the assignment functions \code{\link{nmort}}, \code{\link{growth}}, \code{\link{mass}}, \code{\link{sr}} and \code{\link{maturity}}.
#' 
#' @slot iter the number of Monte-Carlo iterations
#' @slot ainf the asymptotic age (equal to the maximum age for each life-history data vector)
#' @slot sr the type of stock-recruitment relationship used, either Beverton-Holt (\code{'BH'}) or Ricker (\code{'RK'})
#' @slot lhdat list of data matrices containing life-history data-at-age, specifically, mass, maturity, natural mortality and survivorship, as well as a vector of steepness values
#' 
setClass("lhm", slots = list(iter = "numeric", ainf = "numeric", sr = "character", lhdat = "list"))