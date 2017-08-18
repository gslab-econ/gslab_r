#' A Reference Class that Provides a Template for Model Estimation Output
#' @description This class only provides the basic elements of the model estimation output.
#' Subclasses should be created according to different models.
#' @field param Locally optimal primal solution.
#' @field fval The value of objective function at the optimal solution.
#' @field convergence An integer indicating convergence of estimation. 0 indicates successful completion.
#' @field message A character string giving any additional information returned by the optimizer.
#' @field estopts A \code{ModelEstimationOptions} object.
#' @field model A \code{Model} object used for estimation.
#' @field nobs The number of observations in the data used for estimation.
#' @field vcov Variance-covariance matrix of parameters.
#' @field se Standard errors of parameters.
#' @export ModelEstimationOutput
#' @exportClass ModelEstimationOutput
#' @import methods
#' 
ModelEstimationOutput <- setRefClass(Class  = "ModelEstimationOutput",
                                     fields = list(param       = "numeric",
                                                   fval        = "numeric",
                                                   convergence = "numeric",
                                                   message     = "character",
                                                   estopts     = "ModelEstimationOptions",
                                                   model       = "Model",
                                                   nobs        = "numeric",
                                                   vcov        = "matrix",
                                                   se          = "numeric"
                                     ),
                                     methods = list(
                                         initialize = function(slvr, model, data, estopts) {
                                             .self$param       <- slvr$par
                                             .self$fval        <- slvr$value
                                             .self$convergence <- slvr$convergence
                                             .self$message     <- slvr$message
                                             .self$estopts     <- estopts
                                             .self$model       <- model
                                             .self$nobs        <- data$nobs
                                         }
                                     )
)
