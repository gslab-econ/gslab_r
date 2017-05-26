source("../model.R")
source("exampleData.R")
source("exampleEstimationOutput.R")

exampleModel <- setRefClass("exampleModel", contains = "model",
                            fields  = list(include_constant = "numeric"),
                            methods = list(
                              initialize = function(lhs = NULL, rhs = NULL, constant = 1,
                                                    startparams = NULL, suffix = "_coeff") {
                                if (is.null(lhs)) return (.self)
                                if (constant == 0) {
                                  .self$paramlist <- paste(rhs, suffix, sep = "")
                                } else {
                                  .self$paramlist <- paste(c("constant", paste(rhs, suffix, sep = "")))
                                }
                                if (is.null(startparams)) {
                                  .self$startparam <- rep(0, length(.self$paramlist))
                                } else {
                                  .self$startparam = startparams
                                }
                                .self$nparam           <- length(.self$paramlist)
                                .self$lhslist          <- lhs
                                .self$rhslist          <- rhs
                                .self$include_constant <- constant
                                validObject(.self)
                              },
                              
                              estimate = function(data) {
                                est <- optim(.self$startparam, function(x) return(x %*% x))
                                est <- exampleEstimationOutput(est, .self, data)
                                return(est)
                              }
                            )

)

setValidity("exampleModel", function(object) {
  if (length(object$startparam) != object$nparam) {
    stop("Incorrectly specified default_startparam.")
    } else TRUE
}
)