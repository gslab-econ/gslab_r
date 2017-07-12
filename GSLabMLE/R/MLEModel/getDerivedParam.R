#' Compute all derived parameters of the model.
#' @description This method is a wrapper of the method \code{derivedParam}, which returns a vector
#' of all derived parameters.
#' @param .self An \code{MLEModel} object.
#' @param param A vector of parameters at which to calculate the derived parameters.
#' @param constants A list of constants used to to calculate the derived parameters.
#' 
getDerivedParam <- function(.self, param, constants = NULL) {
    if (.self$ndparam == 0) {
        return (numeric(0))
    }
    dparam <- rep(0, .self$ndparam)
    for (i in 1:.self$ndparam) {
        dparam[i] <- .self$derivedParam(param, .self$dparamlist[i], constants)
    }
    return (dparam)
}

