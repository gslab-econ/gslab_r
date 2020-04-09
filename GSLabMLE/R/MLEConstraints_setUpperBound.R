#' @include MLEConstraints.R
MLEConstraints$methods(
    setUpperBound = function(constr_paramlist, bounds) {
        "\\subsection{Description}{
        Update the upper bound constraints of parameters.}\n
        \\subsection{Parameters}{
        \\code{constr_paramlist}: A subset of \\code{paramlist} containing the parameters to be constrained.\n
        \\code{bounds}: A vector with the same length as \\code{constr_paramlist} that defines
        the upper bounds for all parameters in \\code{constr_paramlist}.\n}"
        ncparam <- length(constr_paramlist)
        if (ncparam != length(bounds)) {
            stop("The dimension of the bounds does not match the dimension of the parameters")
        }
        if (any(!constr_paramlist %in% .self$paramlist)) {
            stop("Names not in the list of parameters")
        }
        if (!length(.self$upper)) {
            .self$upper <- rep(Inf, .self$nparam)
        }
        for (i in 1:ncparam) {
            .self$upper[.self$indices[[constr_paramlist[i]]]] <- bounds[i]
        }
    }
)
