source("../ModelEstimationOutput.R")

# output for a simple linear model
ExampleEstimationOutput<- setRefClass("ExampleEstimationOutput", contain = "ModelEstimationOutput",
                                      fields  = list(vcov = "matrix",   # Variance-covariance matrix of parameters
                                                     se   = "numeric"   # Standard errors of parameters
                                      ),
                                      methods = list(
                                          initialize = function(est, model, data) {
                                              .self$param       <- est$par
                                              .self$value       <- est$value
                                              .self$convergence <- est$convergence
                                              .self$model       <- model
                                              .self$nobs        <- data$nobs
                                              .self$vcov        <- diag(model$nparam)
                                              .self$se          <- sqrt(diag(.self$vcov))
                                          }
                                      )
)