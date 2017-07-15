computeLikelihoodByGroup <- function(.self, param, data, nodes, weights) {
    if (model$numerical_integral) {
        unobs <- model$transformUnobservables(param, data, nodes$values)
        data$addData(unobs)
    }
    condik <- model$computeConditionalLikelihoodVector(param, data)
    if (length(data$groupvar) & data$ngroup < data$nobs) {
        grouplik <- prodWithin(condlik, data$groupvar)
    } else {
        grouplik <- condlik
    }
    
    if (model$numerical_integral) {
        grouplok <- sunwithin(grouplik * weights$wgt, weights$group)
    }
    grouplik[grouplik <= 0] <- eps
    return (grouplik)
}