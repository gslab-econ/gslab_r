#' @include MLEModel.R
MLEModel$methods(
    getDerivedParam = function(param, constants = NULL) {
        "\\subsection{Description}{
        Compute all derived parameters of the model.\n
        This method is a wrapper of the method \\code{derivedParam}, which returns a vector
        of all derived parameters.}
        \\subsection{Parameters}{
        \\code{param}: A vector of parameters at which to calculate the derived parameters.\n
        \\code{constants}: A list of constants used to calculate the derived parameters.\n}
        \\subsection{Return}{
        A vector of all derived parameters.}"
        if (.self$ndparam == 0) {
            return (numeric(0))
        }
        dparam <- rep(0, .self$ndparam)
        for (i in 1:.self$ndparam) {
            dparam[i] <- .self$derivedParam(param, .self$dparamlist[i], constants)
        }
        return (dparam)
    }
)
