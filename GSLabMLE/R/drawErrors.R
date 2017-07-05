drawErrors <- function(.self, data) {
    draws <- list()
    for (i in 1:length(.self$error_list)) {
        var_name    <- .self$error_list[i]
        var_dist_fn <- .self$error_distributions[[var_name]]
        if (.self$error_dimensions[[var_name]] > 1) {
            stop("Sorry, we only allow one-dimensional error now. Waiting for update.")
        }
        draws[[var_name]] <- var_dist_fn(runif(data$nobs)) 
    }
    return (draws)
}