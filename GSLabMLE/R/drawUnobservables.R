#' Draws unobservable vectors for an \code{MLEModel} object.
#' @param .self An \code{MLEModel} object.
#' @param data An \code{MLEData} object.
#' @param simopts An \code{MLESimulationOptions} object.
#' 
drawUnobservables <- function(.self, data, simopts) {
    set.seed(simopts$seed)
    draws <- list()
    if (length(.self$group_unobs_list)) {
        if (length(data$groupvar) == 0) {
            stop("Group variable is not set in the data.")
        }
        for (i in 1:length(.self$group_unobs_list)) {
            group_draws <- rnorm(data$ngroup)
            draws[[.self$group_unobs_list[i]]] <- group_draws[data$groupvar]
        }
    }
    if (length(.self$indiv_unobs_list)) {
        for (i in 1:length(.self$indiv_unobs_list)) {
            draws[[.self$indiv_unobs_list[i]]] <- rnorm(data$nobs)
        }
    }
    return (draws)
}