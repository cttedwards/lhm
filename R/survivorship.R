#'
#' Survivorship accessor function
#' 
#' This function can be used to access the survivorship at age contained within a \code{lhm} object. The survivorship is calculated following an assignment of the natural mortality (using \code{\link{nmort}}) and assumed to be one for the first age class. 
#' 
#' @param object a \code{lhm} class object
#' 
#' @examples
#' # M at age vector input
#' dat <- lhm(ainf = 10,iter=1)
#' nmort(dat) <- c(0.1,0.1,0.2)
#' nmort(dat)
#' 
#' # extract survivorship
#' survivorship(dat)
#' 
#' @import methods
#' @include lhm-class.R
#' 
#{{{ accessor function
#' @export
setGeneric("survivorship", function(object, ...) standardGeneric("survivorship"))
#' @rdname survivorship
setMethod("survivorship",signature(object = "lhm"), function(object) return(object@lhdat$survivorship))
#}}}
