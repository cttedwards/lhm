#'
#' Calculate the intrinsic growth rate
#' 
#' A value for the intrinsic growth rate \eqn{r} is calculated using life-history data as a numerical solution to the Euler-Lotka equation.
#' 
#' @param .Object an \code{lhm} object
#' 
#' @return A \code{\link{prior}} class object containing a vector of values for \eqn{r}.
#'
#' @examples
#' # initialise lhm object
#' ainf <- 100
#' iter <- 1
#' rdat <- lhm(ainf,iter)
#' 
#' # assign life-history data
#' nmort(rdat)    <- list(mu=0.18)
#' size(rdat)     <- list(mu=list(Linf=106.5,k=0.229,t0=0.01)) 
#' mass(rdat)     <- list(mu=list(a=1.7e-9,b=3.328))
#' sr(rdat)       <- list(type='BH',mu=0.9)
#' maturity(rdat) <- list(mu=list(acrit=8))
#' 
#' # calculate r
#' r <- rCalc(rdat)
#' 
#' @include prior-class.R
#' @include lhm-class.R
#' @include lhmIter-class.R
#' 
#' @export
setGeneric("rCalc", function(.Object, ...) standardGeneric("rCalc"))
#'
#' @rdname rCalc
setMethod("rCalc", signature = "lhm", function(.Object) {
  
  n <- .Object@iter
  rPrior <- prior(n)
  
  for (i in 1:n) {
    
      rPrior[i] <- rCalc(iteration(.Object, i))
      
      rPrior@generation.time[i] <- tCalc(iteration(.Object, i), rPrior[i])
  }
  
  # calculate log-normal pars
  x <- rPrior@.Data
  
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
  rPrior@lognormal.par <- list('E[log(x)]' = mu, 'SD[log(x)]' = sigma, 'E[x]' = theta, 'VAR[x]' = nu, 'CV[x]' = cv)
  
  return(rPrior)
  
})

setMethod("rCalc", signature = "lhmIter", function(.Object) {
  
  # survivorship
  l <- .Object@lhdat[['survivorship']]
  
  # spawning biomass per recruit
  SBPR  <- sum(.Object@lhdat[['survivorship']] * .Object@lhdat[['mass']] * .Object@lhdat[['maturity']])
  #cat('SBPR:',SBPR,'\n')
  
  # recruits per unit of spawning biomass
  if (.Object@sr == 'BH') {
    alpha <- (4 * .Object@lhdat[['h']])/(SBPR * (1 - .Object@lhdat[['h']]))
  } else if (.Object@sr == 'RK') {
    alpha <- .Object@lhdat[['h']]^1.25 / (SBPR * exp(log(0.2)/0.8))
  } else stop("sr must be either 'BH' or 'RK'\n")
  #cat('alpha:',alpha,'\n')
  
  # female fecundity at age 
  # (recruited mature biomass per unit of spawning biomass)
  m <- alpha * .Object@lhdat[['mass']] * .Object@lhdat[['maturity']]
  
  # minimise Euler-Lotka equation
  obj <- function(r) sum(exp(-r * 1:.Object@ainf) * l * m) - 1
  r <- uniroot(obj,interval = c(0,10))$root
  
  # return intrinsic growth rate
  return(r)
  
})

setMethod("tCalc", signature = "lhmIter", function(.Object, r) {
  
  # survivorship
  l <- .Object@lhdat[['survivorship']]
  
  # spawning biomass per recruit
  SBPR  <- sum(.Object@lhdat[['survivorship']] * .Object@lhdat[['mass']] * .Object@lhdat[['maturity']])
  #cat('SBPR:',SBPR,'\n')
  
  # recruits per unit of spawning biomass
  if (.Object@sr == 'BH') {
    alpha <- (4 * .Object@lhdat[['h']])/(SBPR * (1 - .Object@lhdat[['h']]))
  } else if (.Object@sr == 'RK') {
    alpha <- .Object@lhdat[['h']]^1.25 / (SBPR * exp(log(0.2)/0.8))
  } else stop("sr must be either 'BH' or 'RK'\n")
  #cat('alpha:',alpha,'\n')
  
  # female fecundity at age 
  # (recruited mature biomass per unit of spawning biomass)
  m <- alpha * .Object@lhdat[['mass']] * .Object@lhdat[['maturity']]
  
  # return generation time
  return(sum(exp(-r * 1:.Object@ainf) * l * m * 1:.Object@ainf))
  
})
#}}}
