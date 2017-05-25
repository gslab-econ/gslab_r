source("../ModelEstimationOutput.R")

# output for a simple linear model
ExampleEstimationOutput<- setClass("ExampleEstimationOutput",
                                   slot    = c(vcov = "matrix",
                                               se   = "numeric"),
                                   contain = "ModelEstimationOutput"
)

# create ExampleEstimationOutput method (initialize)
setGeneric(name = "ExampleEstimationOutput",
           def  = function(est, model, data) {
             standardGeneric("ExampleEstimationOutput")
           }
)
setMethod(f = "ExampleEstimationOutput", signature = c("list", "Model", "ModelData"),
          definition = function(est, model, data) {
            obj = new("ExampleEstimationOutput",
                      param = est$par,
                      model = model,
                      nobs  = data@nobs,
                      vcov  = diag(model@nparam))
            obj@se = sqrt(diag(obj@vcov))
            return (obj)
          }
)