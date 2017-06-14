#' A Reference Class that Provides a Template for Model Estimation Output.
#' @description This class only provides the basic elements of the model estimation output.
#' Subclasses should be created according to different models.
#' @field param Estimated parameters
#' @field value The value of objective function at estimated parameters
#' @field convergence A flag for convergence
#' @field model A Model object used for estimation
#' @field nobs The nnumber of observations in the data used for estimation
#' @field vcov Variance-covariance matrix of parameters
#' @field se Standard errors of parameters
#' @export ModelEstimationOutput
#' @exportClass ModelEstimationOutput
#' @import methods
#' 
ModelEstimationOutput <- setRefClass(Class  = "ModelEstimationOutput",
                                     fields = list(param       = "numeric",
                                                   value       = "numeric",
                                                   convergence = "numeric",
                                                   model       = "Model",
                                                   nobs        = "numeric",
                                                   vcov        = "matrix",
                                                   se   = "numeric"
                                     )
)
