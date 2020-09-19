#' 
#' @rdname prior-class
#' 
#' @param x either an integer specifiying the length of an empty vector or a vector of derived values
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 100
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- prior(x)
#'
#' @include prior-class.R
#' 
#' @export
prior <- function(x, ...) new("prior", x, ...)

setMethod("initialize", "prior", function(.Object, x) {
    
    if (missing(x)) .Object@.Data <- numeric()
    else {
        if (length(x) > 1) {
        
            x <- as.numeric(x)
        
            .Object@.Data <- x
            
            # calculate log-normal pars
            
            # transform to normal
            y <- log(x)
            
            # estimate parameters of
            # normal distribution log(x)
            mu     <- mean(y)
            sigma  <- sd(y)
            sigma2 <- sigma^2
            
            # estimate parameters of
            # log-normal distribution
            theta <- exp(mu + sigma2/2)
            nu    <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
            cv    <- sqrt(exp(sigma2) - 1)
            
            # assign
            .Object@lognormal.par <- list('E[log(r)]' = mu, 'SD[log(r)]' = sigma, 'E[r]' = theta, 'VAR[r]' = nu, 'CV[r]' = cv)
            
        } else {
            .Object@.Data <- numeric(x)
        }
    }
    return(.Object)
})

