#' A Reference Class that Holds a List of Estimation Results
#' @field estimates A list of \code{MLEEstimationOutput} objects.
#' @field nest The number of estimates.
#' @field param_list A list of parameter estimates.
#' @field dparam_list A list of of derived parameter estimates.
#' @field startparam_list A list of starting parameters.
#' @field dstartparam_list A list of derived parameters implied by starting parameters.
#' @import methods
#' @export MLESetOfEstimates
#' @exportClass MLESetOfEstimates
MLESetOfEstimates <- setRefClass(Class   = "MLESetOfEstimates",
                                 fields  = list(estimates        = "list",
                                                nest             = "numeric",
                                                param_list       = "list",
                                                dparam_list      = "list",
                                                startparam_list  = "list",
                                                dstartparam_list = "list"
                                 ),
                                 methods = list(
                                     initialize = function(estimates) {
                                         .self$estimates <- estimates
                                         .self$nest      <- length(.self$estimates)
                                         for (i in 1:.self$nest) {
                                             .self$param_list[[i]]       <- .self$estimates[[i]]$param
                                             .self$dparam_list[[i]]      <- .self$estimates[[i]]$dparam
                                             .self$startparam_list[[i]]  <- .self$estimates[[i]]$estopts$startparam
                                             .self$dstartparam_list[[i]] <- .self$estimates[[i]]$model$getDerivedParam(
                                                 .self$startparam_list[[i]], .self$estimates[[i]]$const)
                                         }
                                     }
                                 )
)
