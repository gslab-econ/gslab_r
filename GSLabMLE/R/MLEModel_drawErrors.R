#' @include MLEModel.R
MLEModel$methods(
    drawErrors = function(data, simopts) {
        "\\subsection{Description}{
        Draws error vectors for an \\code{MLEModel} object.}\n
        \\subsection{Parameters}{
        \\code{data}: An \\code{MLEData} object.\n
        \\code{simopts}: An \\code{MLESimulationOptions} object.}"
        set.seed(simopts$seed)
        draws <- list()
        if (length(.self$error_list)) {
            for (i in 1:length(.self$error_list)) {
                var_name    <- .self$error_list[i]
                var_dist_fn <- .self$error_distributions[[var_name]]
                row <- data$nobs
                col <- .self$error_dimensions[[var_name]]
                rand <- matrix(runif(row * col), row, col)
                draws[[var_name]] <- var_dist_fn(rand)
            }
        }
        return (draws)
    }
)
