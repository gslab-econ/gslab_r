#' @include MLEConstraints.R
MLEConstraints$methods(
    jacobianOfConstraints = function(param) {
        "\\subsection{Description}{
        Returns the jacobian of constraints.}\n
        \\subsection{Parameters}{
        \\code{param}: The parameter vector at which to evaluate Jacobian matrix.\n
        \\code{xtol}: A scalar controlling the step size of numerical derivatives.}\n
        \\subsection{Return}{
        A list that contains the Jacobians of different constraints.}"
        J <- list()
        if (length(.self$lower)) {
            J[["lower"]] <- diag(length(.self$lower))
        }
        if (length(.self$upper)) {
            J[["upper"]] <- diag(length(.self$lower)) 
        }
        return (J)
    }
)