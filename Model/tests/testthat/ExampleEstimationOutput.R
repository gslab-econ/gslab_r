ExampleEstimationOutput<- setRefClass(Class    = "ExampleEstimationOutput",
                                      contains = "ModelEstimationOutput")

ExampleEstimationOutput$methods(
    initialize = function(est, model, data) {
        .self$param       <- est$par
        .self$value       <- est$value
        .self$convergence <- est$convergence
        .self$model       <- model
        .self$nobs        <- data$nobs
        .self$vcov        <- diag(model$nparam)
        .self$se          <- sqrt(diag(.self$vcov))
    }
)