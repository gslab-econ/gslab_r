SimpleModel <- setRefClass(Class    = "SimpleModel",
                           contains = "MLEModel"
)

SimpleModel$methods(
    initialize = function(varname) {
        "Model: epsilon ~ N(mu, sigma)"
        obj <- MLEModel(paramlist          = c("mu", "sigma"),
                        lhslist            = varname, # one-dimensional outcome variable
                        startparam         = c(0, 1),
                        error_list         = "epsilon",
                        error_distribution = list("epsilon" = function(p) qnorm(p, 0, 1)),
                        error_dimensions   = list("epsilon" = 1),
                        dparamlist         = c("lnsigma", "CV"))
        for (field in names(obj$getRefClass()$fields())) {
            .self$field(field, obj$field(field))
        }
    },
    
    computeConditionalLikelihoodVector = function(param, data) {
        dnorm(data$var[[.self$lhslist[1]]], param[.self$indices$mu], param[.self$indices$sigma])
    },
    
    computeOutcomes = function(param, data) {
        lhs <- NULL
        lhs[[.self$lhslist]] <-  param[.self$indices$mu] + param[.self$indices$sigma] * data$var$epsilon
        return (lhs)
    },
    
    derivedParam = function(param, paramname, constants = NULL) {
        switch(paramname,
               "lnsigma" = log(param[.self$indices$sigma]),
               "CV"      = param[.self$indices$sigma] / param[.self$indices$mu])
    }
)
