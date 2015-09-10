#'
#' Extract single iteration from \code{lhm} object
#' 
#' Extracts a single iteration from an \code{lhm} object.
#' 
#' @param object an \code{lhm} object
#' @param iter iteration to be extracted
#' 
#' @return An object of class \code{\link{lhmIter}} which contains a single iteration only.
#' 
#' @include lhm-class.R
#' @include lhmIter-class.R
#' 
#' @export
setGeneric("iteration", function(object, iter, ...) standardGeneric("iteration"))
#'
#' @rdname iteration
setMethod("iteration", signature(object = "lhm",iter = "numeric"),
          definition = function(object, iter) {
              x <- new('lhmIter', amax = object@amax, sr = object@sr)
              x@lhdat <- lapply(object@lhdat, function(x) x[, iter]) 
              return(x)
              }
)

