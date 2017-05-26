modelEstimationOutput <- setRefClass("modelEstimationOutput",
                                     fields = list(param = "numeric",
                                                   model = "model",
                                                   data  = "modelData",
                                                   nobs  = "numeric")
)
