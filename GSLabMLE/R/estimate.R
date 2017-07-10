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
    f <- function(param) sumLoglik(.self, param, data)
    slvr <- knitro(x0 = estopts$startparam,
                   objective = f,
                   constraints = estopts$constr$con,
                   xL = estopts$constr$xL,
                   xU = estopts$constr$xU,
                   cL = estopts$constr$cL,
                   cU = estopts$constr$cU)
    est <- MLEEstimationOutput(slvr, .self, data, estopts)
    return (est)
}

logLik <- function(.self, param, data) {
    return (log(.self$computeConditionalLikelihoodVector(param, data)))
}
sumLoglik <- function(.self, param, data) {
    return (-sum(logLik(.self, param, data)))
}
