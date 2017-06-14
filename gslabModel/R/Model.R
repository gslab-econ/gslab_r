#' A reference class that provides a template for models
#' @description This class only provides the basic elements of a model. Subclasses should be
#' created to accommodate different situations and goals.
#' @field paramlist a vector of parameter names
#' @field nparam the number of parameters
#' @field startparam a vector of starting parameter values for estimation
#' @field indices a list giving the index of each parameter
#' @field lhslist names of dependent (i.e., stochastic) variables
#' @field rhslist names of independent (i.e., non-stochastic) variables
#' 
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
    
    XBeta = function(varlist, data, param, include_constant = 0,
                     coef_prefix = "", datavar_suffix = "", constname = "constant") {
        "Return X * beta"
        xbeta <- rep(0, data$nobs)
        for (name in varlist) {
            coeffname <- paste(coef_prefix, name, "_coeff", sep = "")
            dataname  <- paste(name, datavar_suffix, sep = "")
            xbeta     <- xbeta + param[.self$indices[[coeffname]]] * data$var[[dataname]]
        }
        if (include_constant)
            xbeta = xbeta + param[.self$indices[[paste(coef_prefix, constname, sep = "")]]]
        return (xbeta)
    }
)
