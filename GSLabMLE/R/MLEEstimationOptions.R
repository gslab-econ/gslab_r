#' A Reference Class that Defines Options for the \code{estimate} Method of \code{MLEModel} Class
#' @description This class is based on the \code{optim} optimization method.
#' @field startparam Starting paramter vector.
#' @field hesstol Numerical step for computing the Hessian.
#' @field quadacc Accuracy of quadrature nodes for numerical integration in estimation.
#' @field quadacc_deriv Accuracy of quadrature nodes for numerical integration in computing Hessians, gradients, etc.
#' @field constr An \code{MLEConstraints} object.
#' @field first_step_vcov Variance-Covariance matrix for first estimation step
#' @field first_step_paramlist A list of parameter names from first estimation step.
#' @field first_step_param A vector with the values of the first step parameters.
#' @field compute_hessian Compute Hessian at the estimated parameters.
#' @field compute_jacobian Compute Jacobian of likelihood vector at the estimated parameters.
#' @import methods GSLabModel
#' @importClassesFrom GSLabModel ModelEstimationOptions
#' @inheritSection GSLabModel::ModelEstimationOptions Fields
#' @export MLEEstimationOptions
#' @exportClass MLEEstimationOptions
MLEEstimationOptions <- setRefClass(Class    = "MLEEstimationOptions",
                                    contains = "ModelEstimationOptions",
                                    fields   = list(quadacc              = "numeric",
                                                    quadacc_deriv        = "numeric",
                                                    constr               = "MLEConstraints",
                                                    first_step_vcov      = "matrix",
                                                    first_step_paramlist = "list",
                                                    first_step_param     = "numeric",
                                                    compute_hessian      = "numeric", 
                                                    compute_jacobian     = "numeric"
                                    ),
                                    methods = list(
                                        initialize = function(startparam           = numeric(0),
                                                              hesstol              = 1e-4,
                                                              constr               = MLEConstraints(),
                                                              quadacc              = 3,
                                                              quadacc_deriv        = 4,
                                                              first_step_vcov      = matrix(numeric(0), 0, 0),
                                                              first_step_paramlist = list(),
                                                              first_step_param     = as.numeric(NULL),
                                                              compute_hessian      = 1, 
                                                              compute_jacobian     = 1) {
                                            .self$startparam           <- startparam
                                            .self$hesstol              <- hesstol
                                            .self$constr               <- constr
                                            .self$quadacc              <- quadacc
                                            .self$quadacc_deriv        <- quadacc_deriv
                                            .self$first_step_vcov      <- first_step_vcov
                                            .self$first_step_paramlist <- first_step_paramlist
                                            .self$first_step_param     <- first_step_param
                                            .self$compute_hessian      <- compute_hessian
                                            .self$compute_jacobian     <- compute_jacobian
                                        }
                                    )
)
