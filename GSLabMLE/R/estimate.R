estimate <- function(.self, data) {
    f <- function(param) sumLoglik(.self, param, data)
    estopt <- optim(.self$startparam, f)
    return (estopt)
}

logLik <- function(.self, param, data) {
    return (log(.self$computeConditionalLikelihoodVector(param, data)))
}
sumLoglik <- function(.self, param, data) {
    return (-sum(logLik(.self, param, data)))
}