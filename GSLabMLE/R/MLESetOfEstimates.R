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
                                     initialize = function(estimates = NULL) {
                                         .self$estimates <- list()
                                         if (!is.null(estimates)) {
                                             for (i in 1:length(estimates)) {
                                                 .self$estimates[[i]]        <- estimates[[i]]
                                                 .self$param_list[[i]]       <- .self$estimates[[i]]$param
                                                 .self$dparam_list[[i]]      <- .self$estimates[[i]]$dparam
                                                 .self$startparam_list[[i]]  <- .self$estimates[[i]]$estopts$startparam
                                                 .self$dstartparam_list[[i]] <- .self$estimates[[i]]$model$getDerivedParam(
                                                     .self$startparam_list[[i]], .self$estimates[[i]]$const)
                                             }
                                         }
                                         .self$nest <- length(.self$estimates)
                                     }
                                 )
)

MLESetOfEstimates$methods(
    addEstimate = function(estimate) {
        "\\subsection{Description}{
        Add a new \\code{MLEEstimationOutput} object in the list of estimates.}\n
        \\subsection{Parameters}{
        \\code{estimate}: An \\code{MLEEstimationOutput} object to be added.}"
        .self$nest <- .self$nest + 1
        .self$estimates[[.self$nest]] <- estimate
        .self$param_list[[.self$nest]]       <- estimate$param
        .self$dparam_list[[.self$nest]]      <- estimate$dparam
        .self$startparam_list[[.self$nest]]  <- estimate$estopts$startparam
        .self$dstartparam_list[[.self$nest]] <- estimate$model$getDerivedParam(
            .self$startparam_list[[.self$nest]], estimate$const)
    }
)