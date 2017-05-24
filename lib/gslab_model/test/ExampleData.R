source("../ModelData.R")

ExampleData <- setClass("ExampleData",
                        contain = "ModelData"
)

# create ExampleData method (initialize)
setGeneric(name = "ExampleData",
           def  = function(data, varnames = NULL) {
             standardGeneric("ExampleData")
           }
)
setMethod(f = "ExampleData",
          definition = function(data, varnames = NULL) {
            obj = ModelData(data, varnames)
            obj = new("ExampleData",
                      var      = obj@var,
                      varnames = obj@varnames,
                      nobs     = obj@nobs,
                      nvars    = obj@nvars)
            return(obj)
          }
)