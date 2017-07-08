ExampleModel <- setRefClass(Class    = "ExampleModel",
                            contains = "MLEModel"
)

ExampleModel$methods(
    initialize = function(varname) {
        obj <- MLEModel(paramlist          = c("mu", "sigma"),
                        lhslist            = varname, # one-dimensional outcome variable
                        startparam         = c(0, 1),
                        error_list         = "epsilon",
                        error_distribution = list("epsilon" = function(p) qnorm(p, 0, 1)),
                        error_dimensions   = list("epsilon" = 1),
                        indiv_unobs_list   = c("eta", "phi"),
                        dparamlist         = c("lnsigma", "CV"))
        for (field in names(obj$getRefClass()$fields())) {
            .self$field(field, obj$field(field))
        }
    },
    computeConditionalLikelihoodVector = function(param, data) {
        clik <- dnorm(data$var[[.self$lhslist[1]]], param[.self$indices$mu], param[.self$indices$sigma])
        return (clik)
    },
    computeOutcomes = function(param, data) {
        lhs <- NULL
        lhs[[.self$lhslist]] <- param[.self$indices$mu] + param[.self$indices$sigma] * data$var$epsilon
        return (lhs)
    },
    derivedParam = function(param, paramname, constants = NULL) {
        dparam <- switch(paramname,
                         "lnsigma" = log(param[.self$indices$sigma]),
                         "CV"      = param[.self$indices$sigma] / param[.self$indices$mu])
        return (dparam)
    }
)
