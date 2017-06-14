#' A reference class that provides a template for model estimation output.
#' @description This class only provides the basic elements of the model estimation output.
#' Subclasses should be created according to different models.
#' @field param Estimated parameters
#' @field value The value of objective function at estimated parameters
#' @field convergence flag for convergence
#' @field model model object used for estimation
#' @field nobs number of observations in the data used for estimation
#' @export ModelEstimationOutput
#' @exportClass ModelEstimationOutput
#' @import methods
#' 
ModelEstimationOutput <- setRefClass(Class  = "ModelEstimationOutput",
                                     fields = list(param       = "numeric",
                                                   value       = "numeric",
                                                   convergence = "numeric",
                                                   model       = "Model",
                                                   nobs        = "numeric"
                                     )
)
