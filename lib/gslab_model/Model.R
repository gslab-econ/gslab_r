Model <- setClass("Model",
                  slots    = c(paramlist          = "character",
                               default_startparam = "numeric",
                               lhslist            = "character",
                               rhslist            = "character",
                               nparam             = "numeric"),
                  validity = function(object) {
                    if (length(object@paramlist) != object@nparam) {
                      return ("Incorrect number of parameters.")
                    } else {
                      return (TRUE)
                    }
                  }
)