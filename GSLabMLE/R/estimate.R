#' Estimate a Maximum Likelihood Estimation Model
#' @param .self An \code{MLEModel} object.
#' @param data An \code{MLEData} object.
#' @param estopt An \code{MLEEstimationOptions} object.
#' @export
#' 
estimate <- function(.self, data, estopts = NULL) {
    "Model"
    if (is.null(estopts)) {
        estopts <- MLEEstimationOptions()
    }
    if (!length(estopts$startparam)) {
        estopts$startparam <- .self$startparam
    }
    f <- function(param) sumLogLik(.self, param, data)
    slvr <- knitro(x0 = estopts$startparam,
                   objective = f,
                   constraints = estopts$constr$con,
                   xL = estopts$constr$xL,
                   xU = estopts$constr$xU,
                   cL = estopts$constr$cL,
                   cU = estopts$constr$cU)
    if (estopts$compute_hessian) {
        slvr$hessian <- compute_hessian(.self, slvr$x, data, estopts)
    }
    if (estopts$compute_jacobian) {
        slvr$jacobian <- compute_jacobian(.self, slvr$x, data, estopts)
    }
    est <- MLEEstimationOutput(slvr, .self, data, estopts)
    return (est)
}

logLik <- function(.self, param, data) {
    return (log(.self$computeConditionalLikelihoodVector(param, data)))
}
sumLogLik <- function(.self, param, data) {
    return (-sum(logLik(.self, param, data)))
}

compute_hessian <- function(.self, param, data, estopts) {
    hessian <- numHess(function(param) -sumLogLik(.self, param, data), param, estopts$hesstol)
    return (hessian)
}

compute_jacobian <- function(.self, param, data, estopts) {
    jacobian <- numJacob(function(param) -logLik(.self, param, data), param, estopts$hesstol)
    return (jacobian)
}
