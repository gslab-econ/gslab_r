source("../modelEstimationOutput.R")

# output for a simple linear model
exampleEstimationOutput<- setRefClass("exampleEstimationOutput", contain = "modelEstimationOutput",
                                      fields  = list(vcov = "matrix",
                                                     se   = "numeric"),
                                      methods = list(
                                        initialize = function(est, model, data) {
                                          .self$param = est$par
                                          .self$model = model
                                          .self$data  = data
                                          .self$nobs  = data$nobs
                                          .self$vcov  = diag(model$nparam)
                                          .self$se    = sqrt(diag(.self$vcov))
                                        }
                                      )
)