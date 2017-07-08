estimate <- function(.self, data, estopts = NULL) {
    if (is.null(estopts)) {
        estopts <- MLEEstimationOptions()
    }
    if (!length(estopts$startparam)) {
        estopts$startparam <- .self$startparam
    }
    f <- function(param) sumLoglik(.self, param, data)
    slvr <- optim(estopts$startparam, f, method = estopts$method, lower = estopts$constr$lower,
                    upper = estopts$constr$upper, control = list(maxit = estopts$maxit))
    est <- MLEEstimationOutput(slvr, estopts, .self, data)
    return (est)
}

logLik <- function(.self, param, data) {
    return (log(.self$computeConditionalLikelihoodVector(param, data)))
}
sumLoglik <- function(.self, param, data) {
    return (-sum(logLik(.self, param, data)))
}
