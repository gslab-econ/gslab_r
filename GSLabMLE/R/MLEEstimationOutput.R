#' A Reference Class that Holds Estimates of the Maximum Likelihood Estimation model
#' @include MLEConstraints.R
#' @field hessian The numerical Hessian matrix evaluated at estimated parameters.
#' @field jacobian The numerical Jacobian matrix evaluated at estimated parameters.
#' @field constr An \code{MLEConstaints} object.
#' @field dparam A vector of derived parameters.
#' @field const A list of constants used in estimation.
#' @import methods GSLabModel
#' @importClassesFrom GSLabModel ModelEstimationOutput
#' @inheritSection GSLabModel::ModelEstimationOutput Fields
#' @export MLEEstimationOutput
#' @exportClass MLEEstimationOutput
MLEEstimationOutput <- setRefClass(Class    = "MLEEstimationOutput",
                                   contains = "ModelEstimationOutput",
                                   fields   = list(hessian  = "matrix",
                                                   jacobian = "matrix",
                                                   const    = "list",
                                                   constr   = "MLEConstraints",
                                                   dparam   = "numeric"
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
