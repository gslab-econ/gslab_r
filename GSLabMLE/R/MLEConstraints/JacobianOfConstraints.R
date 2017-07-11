#' Returns the Jacobian of constraints
#' @param param Parameter vector at which to evaluate Jacobian.
#' @param xtol: A scalar controlling the step size of numerical derivatives.
#' 
JacobianOfConstraints <- function(.self, param, xtol = .self$jacob_tol) {
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
