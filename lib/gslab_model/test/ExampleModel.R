source("../Model.R")
source("ExampleData.R")
source("ExampleEstimationOutput.R")

ExampleModel <- setRefClass(Class    ="ExampleModel",
                            contains = "Model",
                            fields   = list(include_constant = "numeric")
)

ExampleModel$methods(
    initialize = function(lhs = NULL, rhs = NULL, include_constant = 1,
                          startparam = NULL, suffix = "_coeff") {
        if (is.null(lhs))
            return (.self)
        if (include_constant) {
            .self$paramlist <- c(paste(rhs, suffix, sep = ""), "constant")
        } else {
            .self$paramlist <- paste(rhs, suffix, sep = "")
        }
        .self$nparam           <- length(.self$paramlist)
        .self$indices          <- as.list(1:.self$nparam)
        names(.self$indices)   <- .self$paramlist
        .self$lhslist          <- lhs
        .self$rhslist          <- rhs
        .self$include_constant <- include_constant
        if (!is.null(startparam)) {
            .self$startparam <- startparam
        } else {
            .self$startparam <- rep(0, .self$nparam)
        }
        .self$isValidModel()
    },
    
    isValidModel = function() {
        if (length(.self$startparam) != .self$nparam) {
            stop("Incorrectly specified start parameters")
        } else {
            return (TRUE)
        }
    },
    
    estimate = function(data) {
        # Minimizes the objective function X'*X
        
        est <- optim(.self$startparam, 
                     function(x) return(x %*% x))
        est <- ExampleEstimationOutput(est, .self, data)
        return (est)
    }
)
