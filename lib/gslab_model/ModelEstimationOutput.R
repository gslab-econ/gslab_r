ModelEstimationOutput <- setRefClass(Class  = "ModelEstimationOutput",
                                     fields = list(param       = "numeric",  # estimated parameters
                                                   value       = "numeric",  # value of objective function at estimated parameters
                                                   convergence = "numeric",  # flag for convergence
                                                   model       = "Model",    # model object used for estimation
                                                   nobs        = "numeric"   # number of observations in the data used for estimation
                                     )
)
