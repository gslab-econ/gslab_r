ExampleModel <- setRefClass(Class    = "ExampleModel",
                            contains = "MLEModel"
)

ExampleModel$methods(
    initialize = function(varname) {
        "Model: eta_j ~ N(mu, sigma), j=1,2,...J, epsilon_i ~ N(eta_j, 1), where i belongs to group j"
        obj <- MLEModel(paramlist          = c("mu", "sigma"),
                        lhslist            = varname, # one-dimensional outcome variable
                        rhslist            = "group",
                        startparam         = c(0, 1),
                        error_list         = "epsilon",
                        error_distribution = list("epsilon" = function(p) qnorm(p, 0, 1)),
                        error_dimensions   = list("epsilon" = 1),
                        group_unobs_list   = "eta",
                        dparamlist         = c("lnsigma", "CV"))
        for (field in names(obj$getRefClass()$fields())) {
            .self$field(field, obj$field(field))
        }
    },
    
    transformUnobservables = function(param, data, unobs) {
        unobs[["eta"]] <- param[.self$indices[["mu"]]] + unobs[["eta"]] * param[.self$indices[["sigma"]]] 
        return (unobs)
    },
    
    computeConditionalLikelihoodVector = function(param, data) {
        clik <- dnorm(data$var[[.self$lhslist[1]]], data$var$eta, 1)
        return (clik)
    },
    
    computeOutcomes = function(param, data) {
        lhs <- NULL
        lhs[[.self$lhslist]] <- data$var$eta + data$var$epsilon * 1
        return (lhs)
    },
    
    derivedParam = function(param, paramname, constants = NULL) {
        dparam <- switch(paramname,
                         "lnsigma" = log(param[.self$indices$sigma]),
                         "CV"      = param[.self$indices$sigma] / param[.self$indices$mu])
        return (dparam)
    }
)
