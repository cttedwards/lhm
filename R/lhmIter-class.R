#' Class containing single iteration of life-history information
#' 
#' This is an S4 object class similar to \code{\link{lhm}}. It differs from \code{lhm} in that life-history data are stored as vectors (rather than matrices) and therefore correspond to a single monte-carlo interation only. The vectorisation is necessary for efficient estimation of derived quantities such as \eqn{r} using \code{\link{rCalc}}.
#'
#' Initialisation is through a call to \code{\link{iteration}}, which will extract a single iteration from an \code{lhm} object.
#' 
#' @slot amax the maximum age for each life-history data vector
#' @slot sr the type of stock-recruitment relationship used, either Beverton-Holt (\code{'BH'}) or Ricker (\code{'RK'})
#' @slot lhdat list of numeric vectors containing life-history data-at-age, specifically, mass, maturity, natural mortality and survivorship, as well as a steepness value
#' 
#' @seealso \code{\link{lhm}}, \code{\link{iteration}}, \code{\link{rCalc}}
#' 
setClass("lhmIter", slots = list(amax = "numeric", sr = "character", lhdat = "list"))