ModelEstimationOutput <- setClass("ModelEstimationOutput",
                                  slots = c(param = "numeric",
                                            model = "Model",
                                            data  = "ModelData",
                                            nobs  = "numeric")
)

# create ModelEstimationOutput method
setGeneric(name = "ModelEstimationOutput",
           def  = function(est, model, data) {
             standardGeneric("ModelEstimationOutput")
           }
)
setMethod(f = "ModelEstimationOutput", signature  = c("list", "Model", "ModelData"),
          definition = function(est, model, data) {
            obj = new("ModelEstimationOutput",
                      param = est$par,
                      model = model,
                      data  = data,
                      nobs  = data@nobs)
            return(obj)
          }
)
