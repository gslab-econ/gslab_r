source("../R/MLEModel.R")
ExampleModel <- setRefClass(Class    = "ExampleModel",
                            contains = "MLEModel"
)

ExampleModel$methods(
    initialize = function(varname) {
        obj <- MLEModel(paramlist          = c("mu", "sigma"),
                        lhslist            = varname,
                        startparam         = c(0, 1),
                        error_list         = "epsilon",
                        error_distribution = list("epsilon" = function(p) qnorm(p, 0, 1)),
                        error_dimensions   = list("epsilon" = 1),
                        group_unobs_list   = c("eta"),
                        indiv_unobs_list   = c("nu"),
                        dparamlist         = c("lnsigma", "CV"))
        for (field in names(obj$getRefClass()$fields())) {
            .self$field(field, obj$field(field))
        }
    },
    computeConditionalLikelihoodVector = function(param, data) {
        clik <- dnorm(data$var[[.self$lhslist]], param[.self$indices$mu], param[.self$indices$sigma])
        return (clik)
    },
    computeOutcomes = function(param, data) {
        lhs <- NULL
        lhs[[.self$lhslist]] <- param[.self$indices$mu] + param[.self$indices$sigma] * data$var$epsilon
        return (lhs)
    }
)
