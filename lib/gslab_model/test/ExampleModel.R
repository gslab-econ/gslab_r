source("../model.R")
source("exampleData.R")
source("exampleEstimationOutput.R")

exampleModel <- setRefClass("exampleModel", contains = "model",
                            fields  = list(include_constant = "numeric"),
                            methods = list(
                              initialize = function(lhs, rhs, constant = 1,
                                                    startparam = NULL, suffix = "_coeff") {
                                if (constant == 1) {
                                  .self$paramlist <- paste(c("constant", paste(rhs, suffix, sep = "")))
                                } else if (constant == 0) {
                                  .self$paramlist <- paste(rhs, suffix, sep = "")
                                } else {
                                  stop("Incorrectly specified include_constant.")
                                }
                                .self$nparam <- length(.self$paramlist)
                                if (is.null(startparam)) {
                                  .self$default_startparam <- rep(0, .self$nparam)
                                } else if (length(startparam) != .self$nparam) {
                                  stop("Incorrectly specified default_startparam.")
                                } else {
                                  .self$default_startparam = startparam
                                }
                                .self$lhslist          = lhs
                                .self$rhslist          = rhs
                                .self$include_constant = constant
                              },
                              
                              estimate = function(data) {
                                est <- optim(.self$default_startparam, function(x) return(x %*% x))
                                est <- exampleEstimationOutput(est, .self, data)
                                return(est)
                              }
                            )

)
