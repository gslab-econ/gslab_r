computeLikelihoodByGroup <- function(.self, param, data, nodes, weights) {
    if (.self$numerical_integral) {
        unobs <- .self$transformUnobservables(param, data, nodes$values)
        data$addData(unobs, replace = TRUE)
    }
    condlik <- .self$computeConditionalLikelihoodVector(param, data)
    if (length(data$groupvar) & data$ngroup < data$nobs) {
        grouplik <- prodWithin(condlik, data$groupvar)$value
    } else {
        grouplik <- condlik
    }
    if (.self$numerical_integral) {
        grouplik <- sumWithin(grouplik * weights$wgt, weights$group)$value
    }
    grouplik[grouplik <= 0] <- exp(1)
    return (grouplik)
}