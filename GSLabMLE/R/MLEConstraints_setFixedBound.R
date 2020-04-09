#' @include MLEConstraints.R
MLEConstraints$methods(
    setFixedBound = function(constr_paramlist, bounds) {
        "\\subsection{Description}{
        Update the fixed value constraints of parameters.}\n
        \\subsection{Parameters}{
        \\code{constr_paramlist}: A subset of \\code{paramlist} containing the parameters to be constrained.\n
        \\code{bounds}: A vector with the same length as \\code{constr_paramlist} that defines
        the fixed constraints for all parameters in \\code{constr_paramlist}.\n}"
        .self$setUpperBound(constr_paramlist, bounds)
        .self$setLowerBound(constr_paramlist, bounds)
    }
)