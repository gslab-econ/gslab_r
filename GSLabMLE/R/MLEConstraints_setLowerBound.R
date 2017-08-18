#' @include MLEConstraints.R
MLEConstraints$methods(
    setLowerBound = function(constr_paramlist, bounds) {
        "\\subsection{Description}{
        Update the lower bound constraints of parameters.}\n
        \\subsection{Parameters}{
        \\code{constr_paramlist}: A subset of \\code{paramlist} containing the parameters to be constrained.\n
        \\code{bounds}: A vector with the same length as \\code{constr_paramlist} that defines
        the lower bounds for all parameters in \\code{constr_paramlist}.\n}"
        ncparam <- length(constr_paramlist)
        if (ncparam != length(bounds)) {
            stop("The dimension of the bounds does not match the dimension of the parameters")
        }
        if (any(!constr_paramlist %in% .self$paramlist)) {
            stop("Names not in the list of parameters")
        }
        if (!length(.self$lower)) {
            .self$lower <- rep(-Inf, .self$nparam)
        }
        for (i in 1:ncparam) {
            .self$lower[.self$indices[[constr_paramlist[i]]]] <- bounds[i]
        }
    }
)