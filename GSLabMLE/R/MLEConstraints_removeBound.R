#' @include MLEConstraints.R
MLEConstraints$methods(
    removeBound = function(constr_paramlist) {
        "\\subsection{Description}{
        Remove upper and lower bound constraints of parameters.}\n
        \\subsection{Parameters}{
        \\code{constr_paramlist}: A subset of \\code{paramlist} containing the parameters whose
        upper and lower bounds are to be removed.}"
        ncparam <- length(constr_paramlist)
        .self$setUpperBound(constr_paramlist, rep(Inf, ncparam))
        .self$setLowerBound(constr_paramlist, rep(-Inf, ncparam))
    }
)