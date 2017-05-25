ModelEstimationOutput <- setClass("ModelEstimationOutput",
                                  slots = c(param = "numeric",
                                            model = "Model",
                                            data  = "ModelData",
                                            nobs  = "numeric")
)