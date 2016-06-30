#' @rdname lhmIter-class
#' 
#' @include lhmIter-class.R
#'
setMethod("initialize", signature = "lhmIter", definition = function(.Object, ainf, sr) {
    
    if (!missing(ainf)) {
        
        .Object@ainf <- ainf
        .Object@lhdat[['survivorship']] <- vector('numeric',ainf)
        .Object@lhdat[['M']]            <- vector('numeric',ainf)
        .Object@lhdat[['size']]         <- vector('numeric',ainf)
        .Object@lhdat[['mass']]         <- vector('numeric',ainf)
        .Object@lhdat[['maturity']]     <- vector('numeric',ainf)
        
    }
    
    if (!missing(sr)) {
        .Object@sr           <- sr
        .Object@lhdat[['h']] <- numeric(1)
    }
    
    .Object
    
})
