#' @include MLEConstraints.R
MLEConstraints$methods(
    jacobianOfConstraints = function(param, xtol = .self$jacob_tol) {
        "\\subsection{Description}{
        Returns the jacobian of constraints.}\n
        \\subsection{Parameters}{
        \\code{param}: The parameter vector at which to evaluate Jacobian matrix.\n
        \\code{xtol}: A scalar controlling the step size of numerical derivatives.}\n
        \\subsection{Return}{
        A list that contains the Jacobians of different constraints.}"
        J <- list()
        if (length(.self$xL)) {
            J[["xL"]] <- diag(length(.self$xL)) 
        }
        if (length(.self$xU)) {
            J[["xU"]] <- diag(length(.self$xU)) 
        }
        if (length(.self$cL)) {
            J[["cL"]] <- numJacob(.self$con, param, xtol)
        }
        if (length(.self$cU)) {
            J[["cU"]] <- numJacob(.self$con, param, xtol)
        }
        return (J)
    }
)