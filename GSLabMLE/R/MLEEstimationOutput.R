#' A Reference Class that Defines Options for the \code{simulate} Method of \code{MLEModel} Class.
#' @field constr An \code{MLEConstaints} object.
#' @field dparam A vector of derived parameters.
#' @field const A list of constants.
#' @export MLESimulationOptions
#' @exportClass MLESimulationOptions
#' 
MLEEstimationOutput <- setRefClass(Class    = "MLEEstimationOutput",
                                   contains = "ModelEstimationOutput",
                                   fields   = list(constr = "MLEConstraints",
                                                   dparam  = "numeric",
                                                   const   = "list"),
                                   methods = list(
                                       initialize = function(slvr, estopts, model, data) {
                                           .self$param       <- slvr$par
                                           .self$value       <- slvr$value
                                           .self$convergence <- slvr$convergence
                                           .self$model       <- model
                                           .self$nobs        <- data$nobs
                                           .self$const       <- data$const
                                           .self$constr      <- estopts$constr
                                           .self$dparam      <- .self$model$getDerivedParam(.self$param, .self$const)
                                       }
                                   )
)
