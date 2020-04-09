#' @include MLEConstraints.R
MLEConstraints$methods(
    isConsistent = function(param, tolerance = 1e-4) {
        "\\subsection{Description}{
        Determines whether parameters satisfy constraints.}\n
        \\subsection{Parameters}{
        \\code{param}: The parameter vector at which to check against constraints.\n
        \\code{xtol}: A scalar controlling the tolerance of tests. Default is 1e-4.}\n
        \\subsection{Return}{
        A logical scalar indicating whether or not parameters satisfy constraints.}"
        is_consistent <- 1
        if (length(.self$lower)) {
            is_consistent <- is_consistent & all(param >= .self$lower - tolerance)
        }
        if (length(.self$upper)) {
            is_consistent <- is_consistent & all(param <= .self$upper + tolerance)
        }
        return (is_consistent)
    }
)