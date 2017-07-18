#' @include MLEModel.R
MLEModel$methods(
    computeLikelihoodByGroup = function(param, data, nodes, weights) {
        "\\subsection{Description}{
        Compute likelihood by group, integrating unobservables numerically.}\n
        \\subsection{Parameters}{
        \\code{param}: The parameters at which to calculate likelihood.\n
        \\code{data}: An \\code{MLEData} object.\n
        \\code{nodes}: A list of nodes for all unobservables to facilitate the numerical integration.\n
        \\code{weights}: A list of weights of each group to facilitate the numerical integration.}
        \\subsection{Return}{
        A vector of likelihood with length equal to the number of groups.}"
        if (.self$numerical_integral) {
            unobs <- .self$transformUnobservables(param, data, nodes$values)
            data$addData(unobs, replace = TRUE)
        }
        condlik <- .self$computeConditionalLikelihoodVector(param, data)
        if (length(data$groupvar) & all(data$ngroup < data$nobs)) {
            grouplik <- prodWithin(condlik, data$groupvar)$value
        } else {
            grouplik <- condlik
        }
        if (.self$numerical_integral) {
            grouplik <- sumWithin(grouplik * weights$wgt, weights$group)$value
        }
        grouplik[grouplik <= 0] <- 1e-300
        return (grouplik)
    }
)