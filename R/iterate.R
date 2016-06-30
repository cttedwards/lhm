#'
#' Add iterations to existing \code{lhm} object
#' 
#' This function will add an iteration dimension to the life-history data contained in an \code{lhm} object.
#' 
#' @param object existing \code{lhm} object
#' @param iter number of iterations
#' 
#' @return Returns an \code{lhm} object with life-history data replicated \code{iter} times
#' 
#' @examples
#' # initialise data object
#' dat <- lhm(ainf = 30, iter = 1)
#' 
#' # to generate monte-carlo iterations first either 
#' # re-initialise 'lhm' object using more iterations or 
#' # iterate existing object:
#' dat <- iterate(dat, iter = 200)
#' 
#' @include lhm-class.R
#'
#' @export
setGeneric("iterate", function(object, iter, ...) standardGeneric("iterate"))
#'
#' @rdname iterate
setMethod("iterate", signature(object = "lhm",iter = "numeric"),
          function(object, iter) new('lhm', lhm = object, iter = iter)
)
