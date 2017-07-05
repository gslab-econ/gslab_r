drawUnobservables <- function(.self, data) {
    draws <- list()
    if (length(.self$group_unobs_list)) {
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