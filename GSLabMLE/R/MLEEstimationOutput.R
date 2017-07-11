#' A Reference Class that Defines Options for the \code{simulate} Method of \code{MLEModel} Class.
#' @field constr An \code{MLEConstaints} object.
#' @field dparam A vector of derived parameters.
#' @field const A list of constants used in estimation.
#' @export MLESimulationOptions
#' @exportClass MLESimulationOptions
#' 
MLEEstimationOutput <- setRefClass(Class    = "MLEEstimationOutput",
                                   contains = "ModelEstimationOutput",
                                   fields   = list(hessian  = "matrix",
                                                   jacobian = "matrix",
                                                   const    = "list",
                                                   dparam   = "numeric",
                                                   constr   = "MLEConstraints"
                                   ),
                                   methods = list(
                                       initialize = function(slvr, model, data, estopts) {
                                           obj <- ModelEstimationOutput(slvr, model, data, estopts)
                                           for (field in names(obj$getRefClass()$fields())) {
                                               .self$field(field, obj$field(field))
                                           }
                                           rm(obj)
                                           .self$hessian  <- slvr$hessian
                                           .self$jacobian <- slvr$jacobian
                                           .self$const    <- data$const
                                           .self$constr   <- estopts$constr
                                           .self$dparam   <- .self$model$getDerivedParam(.self$param, .self$const)
                                       }
                                   )
)
