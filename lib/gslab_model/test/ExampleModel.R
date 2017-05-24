source("../Model.R")
source("ExampleData.R")
source("ExampleEstimationOutput.R")

ExampleModel <- setClass("ExampleModel",
                         slot = c(include_constant = "numeric"),
                         prototype = list(include_constant = 1),
                         validity  = function(object) {
                           if (length(object@paramlist) != object@nparam) {
                             return("Incorrect number of parameters.")
                           }
                           if (object@nparam != length(object@rhslist) + object@include_constant) {
                             if (object@nparam == length(object@rhslist)) {
                               return("Constant is not added in paramer list.")
                             } else {
                               return("Incorrect number of parameters.")
                             }
                             if (!object@include_constant %in% c(0, 1)) {
                               return("Incorrectly specified slot include_constant.")
                             }
                           }
                           return(TRUE)
                         },
                         contains = "Model"
)

# create ExampleModel method
setGeneric(name = "ExampleModel",
           def  = function(lhs, rhs, include_constant = 1,
                           default_startparam = NULL, suffix = "coeff") {
             standardGeneric("ExampleModel")
           }
)
setMethod(f = "ExampleModel", signature = c("character", "character"),
          definition = function(lhs, rhs, include_constant = 1,
                                default_startparam = NULL, suffix = "_coeff") {
            if (include_constant == 1) {
              paramlist <- paste(c("constant", paste(rhs, suffix, sep = "")))
            } else if (include_constant == 0) {
              paramlist <- paste(rhs, suffix, sep = "")
            } else {
              stop("Incorrectly specified include_constant.")
            }
            nparam <- length(paramlist)
            if (is.null(default_startparam)) {
              default_startparam <- rep(0, nparam)
            } else if (length(default_startparam) != nparam) {
              stop("Incorrectly specified default_startparam.")
            }
            obj = new("ExampleModel",
                      paramlist          = paramlist,
                      default_startparam = default_startparam,
                      lhslist            = lhs,
                      rhslist            = rhs,
                      nparam             = nparam,
                      include_constant   = include_constant)
            validObject(obj)
            return(obj)
          }
)

# create estimate method
setGeneric(name = "estimate",
           def  = function(obj, data) {
             standardGeneric("estimate")
           }
)
setMethod(f = "estimate", signature = c("ExampleModel", "ExampleData"),
          definition = function(obj, data) {
            # minimize X'X-X
            g <- function(x) return(x %*% x - rep(1, length(x)) %*% x)
            result <- optim(obj@default_startparam, g)
            est    <- ExampleEstimationOutput(result, obj, data)
            return(est)
          }
)