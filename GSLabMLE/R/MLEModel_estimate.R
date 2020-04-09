#' @include MLEModel.R
MLEModel$methods(
    estimate = function(data, estopts = NULL) {
        "\\subsection{Description}{
        Estimate an \\code{MLEModel} by maximum Likelihood with numerical integration.}\n
        \\subsection{Parameters}{
        \\code{data}: An \\code{MLEData} object.\n
        \\code{estopt}: An \\code{MLEEstimationOptions} object. If not specified, the default
        options will be used.\n}
        \\subsection{Return}{
        An \\code{MLEEstimationOutput} object.}"
        if (is.null(estopts)) {
            estopts <- MLEEstimationOptions()
        }
        if (!length(estopts$startparam)) {
            estopts$startparam <- .self$startparam
        }
        result   <- .self$computeNodesAndWeights(data, estopts$quadacc)
        nodes    <- result$nodes
        weights  <- result$weights
        data_rep <- result$data_rep
        f <- function(param) {
            GSLabMLE:::sumLogLik(.self, param, data_rep, nodes, weights)
        }
        slvr <- optim(par    = estopts$startparam,
                      fn     = f,
                      method = "L-BFGS-B",
                      lower  = estopts$constr$lower,
                      upper  = estopts$constr$upper)
        if (estopts$compute_hessian) {
            slvr$hessian <- GSLabMLE:::computeHessian(.self, slvr$par, data, estopts)
        } else {
            slvr$hessian <- matrix(0, 0, 0)
        }
        if (estopts$compute_jacobian) {
            slvr$jacobian <- GSLabMLE:::computeJacobian(.self, slvr$par, data, estopts)
        } else {
            slvr$jacobian <- matrix(0, 0, 0)
        }
        est <- MLEEstimationOutput(slvr, .self, data, estopts)
        return (est)
    }
)

logLik <- function(model, param, data_rep, nodes, weights) {
    log(model$computeLikelihoodByGroup(param, data_rep, nodes, weights))
}

sumLogLik <- function(model, param, data_rep, nodes, weights) {
    -sum(GSLabMLE:::logLik(model, param, data_rep, nodes, weights))
}

computeHessian <- function(model, param, data, estopts) {
    result   <- model$computeNodesAndWeights(data, estopts$quadacc_deriv)
    nodes    <- result$nodes
    weights  <- result$weights
    data_rep <- result$data_rep
    Hess     <- NumericalDerivatives::numHess(function(param)
        -GSLabMLE:::sumLogLik(model, param, data_rep, nodes, weights),
        param, estopts$hesstol)
    return (Hess)
}

computeJacobian <- function(model, param, data, estopts) {
    result   <- model$computeNodesAndWeights(data, estopts$quadacc_deriv)
    nodes    <- result$nodes
    weights  <- result$weights
    data_rep <- result$data_rep
    Jacob    <- NumericalDerivatives::numJacob(function(param)
        -GSLabMLE:::logLik(model, param, data_rep, nodes, weights),
        param, estopts$hesstol)
    return (Jacob)
}
