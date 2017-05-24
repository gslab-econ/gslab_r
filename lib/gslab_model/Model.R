Model <- setClass("Model",
                  slots     = c(paramlist          = "character",
                                default_startparam = "numeric",
                                lhslist            = "character",
                                rhslist            = "character",
                                nparam             = "numeric"),
                  prototype = list(paramlist          = as.character(NA),
                                   default_startparam = as.numeric(NA),
                                   lhslist            = as.character(NA),
                                   rhslist            = as.character(NA),
                                   nparam             = as.numeric(NA)),
                  validity  = function(object) {
                    if (length(object@paramlist) != object@nparam) {
                      return("Incorrect number of parameters.")
                    }
                    return(TRUE)
                  }
)
