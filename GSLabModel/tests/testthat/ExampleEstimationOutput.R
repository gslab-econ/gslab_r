ExampleEstimationOutput<- setRefClass(Class    = "ExampleEstimationOutput",
                                      contains = "ModelEstimationOutput")

ExampleEstimationOutput$methods(
    initialize = function(slvr, model, data, estopts) {
        obj <- ModelEstimationOutput(slvr, model, data, estopts)
        for (field in names(obj$getRefClass()$fields())) {
            .self$field(field, obj$field(field))
        }
        rm(obj)
        .self$vcov <- diag(model$nparam)
        .self$se   <- sqrt(diag(.self$vcov))
    }
)