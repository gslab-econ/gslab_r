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
            sumLogLik(.self, param, data_rep, nodes, weights)
        }
        slvr <- knitro(x0          = estopts$startparam,
                       objective   = f,
                       constraints = estopts$constr$con,
                       xL          = estopts$constr$xL,
                       xU          = estopts$constr$xU,
                       cL          = estopts$constr$cL,
                       cU          = estopts$constr$cU,
                       options     = append(list(outlev = estopts$outlev),
                                            estopts$knitrotxt))
        if (estopts$compute_hessian) {
            slvr$hessian <- computeHessian(.self, slvr$x, data, estopts)
        } else {
            slvr$hessian <- matrix(0, 0, 0)
        }
        if (estopts$compute_jacobian) {
            slvr$jacobian <- computeJacobian(.self, slvr$x, data, estopts)
        } else {
            slvr$jacobian <- matrix(0, 0, 0)
        }
        est <- MLEEstimationOutput(slvr, .self, data, estopts)
        return (est)
    }
)

logLik <- function(.self, param, data_rep, nodes, weights) {
    log(.self$computeLikelihoodByGroup(param, data_rep, nodes, weights))
}

sumLogLik <- function(.self, param, data_rep, nodes, weights) {
    temp <- -sum(logLik(.self, param, data_rep, nodes, weights))
    print(temp)
    return (temp)
}

computeHessian <- function(.self, param, data, estopts) {
    result   <- .self$computeNodesAndWeights(data, estopts$quadacc_deriv)
    nodes    <- result$nodes
    weights  <- result$weights
    data_rep <- result$data_rep
    return (numHess(function(param)-sumLogLik(.self, param, data_rep, nodes, weights),
                    param, estopts$hesstol))
}

computeJacobian <- function(.self, param, data, estopts) {
    result   <- .self$computeNodesAndWeights(data, estopts$quadacc_deriv)
    nodes    <- result$nodes
    weights  <- result$weights
    data_rep <- result$data_rep
    return (numJacob(function(param) -logLik(.self, param, data_rep, nodes, weights),
                     param, estopts$hesstol))
}