source("../ModelEstimationOutput.R")

ExampleEstimationOutput<- setClass("ExampleEstimationOutput",
                                   slot      = c(vcov = "matrix"),
                                   prototype = list(vcov = as.matrix(NA)),
                                   contain   = "ModelEstimationOutput"
)

# create ExampleEstimationOutput method
setGeneric(name = "ExampleEstimationOutput",
           def  = function(est, model, data) {
             standardGeneric("ExampleEstimationOutput")
           }
)
setMethod(f = "ExampleEstimationOutput", signature = c("list", "Model", "ModelData"),
          definition = function(est, model, data) {
            obj = ModelEstimationOutput(est, model, data)
            obj = new("ExampleEstimationOutput",
                      param = obj@param,
                      model = obj@model,
                      data  = obj@data,
                      nobs  = obj@nobs,
                      vcov  = diag(obj@model@nparam))
            return(obj)
          }
)
