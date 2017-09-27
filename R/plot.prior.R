#'
#' Plot prior distribution
#' 
#' This function provides a simple \code{plot} method for the \code{prior} object class.
#' 
#' @param x an \code{\link{prior}} object class
#' @param ... additional arguments for \code{\link[graphics]{hist}}
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 1000
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- prior(x)
#' 
#' # plot
#' \dontrun{
#' plot(r, xlab = 'r', ylab = '', yaxt = 'n', main = '')
#' }
#' 
#' @include prior-class.R
#' 
#' @importFrom graphics plot
#' 
#' @export
plot.prior <- function(x, ...)
{
    logmu    <- x@lognormal.par[['E[log(x)]']]
    logsigma <- x@lognormal.par[['SD[log(x)]']]
    
    hist(x@.Data, freq = FALSE, ..., main = "", xlab = "")
    curve(dlnorm(x, logmu, logsigma), col = 2, lwd = 2, add = TRUE)
}