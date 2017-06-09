source("../Model.R")
source("ExampleData.R")
source("ExampleEstimationOutput.R")

ExampleModel <- setRefClass("ExampleModel", contains = "Model",
                            fields  = list(include_constant = "numeric"),
                            methods = list(
                                initialize = function(lhs = NULL, rhs = NULL, include_constant = 1,
                                                      startparam = NULL, suffix = "_coeff") {
                                    if (is.null(lhs))
                                        return (.self)
                                    if (include_constant) {
                                        param <- c(paste(rhs, suffix, sep = ""), "constant")
                                    } else {
                                        param <- paste(rhs, suffix, sep = "")
                                    }
                                    .self$paramlist        <- param
                                    .self$nparam           <- length(.self$paramlist)
                                    .self$indices          <- as.list(1:.self$nparam)
                                    names(.self$indices)   <- .self$paramlist
                                    .self$lhslist          <- lhs
                                    .self$rhslist          <- rhs
                                    .self$include_constant <- include_constant
                                    if (!is.null(startparam)) {
                                        if (length(startparam) != .self$nparam) {
                                            stop("Incorrectly specified startparam")
                                        } else {
                                            .self$startparam <- startparam
                                        }
                                    } else {
                                        .self$startparam <- rep(0, length(.self$paramlist))
                                    }
                                }
                            )
)

ExampleModel$methods(
    estimate = function(data) {
        est <- optim(.self$startparam, function(x) return(x %*% x - rep(1, length(x)) %*% x))
        est <- ExampleEstimationOutput(est, .self, data)
        return(est)
    }
)
