#' A Reference Class that Provides a Template for Models
#' @description This class only provides the basic elements of a model. Subclasses should be
#' created to accommodate different situations and goals.
#' @field paramlist A vector of parameter names.
#' @field nparam The number of parameters.
#' @field startparam A vector of starting parameter values for estimation.
#' @field indices A list giving the index of each parameter.
#' @field lhslist The names of dependent (i.e., stochastic) variables.
#' @field rhslist The names of independent (i.e., non-stochastic) variables.
#' @export Model
#' @exportClass Model
#' @import methods
#' 
Model <- setRefClass(Class  = "Model",
                     fields = list(paramlist  = "character",
                                   nparam     = "numeric",
                                   startparam = "numeric",
                                   indices    = "list",
                                   lhslist    = "character",
                                   rhslist    = "character"
                     )
)

Model$methods(
    isValidModel = function() {
        "A method to determine if it's a valid instantiation and should be overridden in subclasses."
        return (TRUE)
    },
    
    isValidParameterVector = function(param) {
        "A method to determine if the parameters are valid for the model.\n
        \\code{param}: A vector of parameters."
        if (length(param) != .self$nparam) {
            stop("Wrong length of parameters provided.")
        }
    },
    
    XBeta = function(varlist, data, param, constant = 0, constname = "constant", coeff_prefix = "",
                     coeff_suffix = "_coeff", datavar_prefix = "", datavar_suffix = "") {
        "Return the product of data and coefficients.\n
        \\code{varlist}: A vector of variables names.\n
        \\code{data}: A ModelData object.\n
        \\code{param}: A vector of coefficients. The order of the coefficients should be the same as 
        the field \\code{varnames} of the \\code{Model} object.\n
        \\code{constant}: Whether the calculation includes constant or not.\n
        \\code{constname}: The name of the constant.\n
        \\code{coeff_prefix}: The prefix of coefficient names in the model.\n
        \\code{coeff_suffix}: The suffix of coefficient names in the model.\n
        \\code{datavar_prefix}: The prefix of variable names in the dataset.\n
        \\code{datavar_suffix}: The suffix of variable names in the dataset.\n"
        
        .self$isValidParameterVector(param)
        xbeta <- rep(0, data$nobs)
        for (name in varlist) {
            coeffname <- sprintf("%s%s%s", coeff_prefix, name, coeff_suffix)
            dataname  <- sprintf("%s%s", name, datavar_suffix)
            if (!dataname %in% data$varnames) {
                stop(sprintf("%s is not in the dataset", dataname))
            } else if (!coeffname %in% names(.self$indices)) {
                stop(sprintf("%s is not in the model", coeffname))
            }
            xbeta <- xbeta + param[.self$indices[[coeffname]]] * data$var[[dataname]]
        }
        if (constant) {
            coeffname <- sprintf("%s%s", coeff_prefix, constname)
            if (!coeffname %in% names(.self$indices)) {
                stop(sprintf("%s is not in the model", coeffname))         
            }
            xbeta <- xbeta + param[.self$indices[[coeffname]]]
        }
        return (xbeta)
    }
)
