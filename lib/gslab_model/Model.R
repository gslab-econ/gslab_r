Model <- setRefClass(Class  = "Model",
                     # Model: A class that provides a template for models
                     
                     fields = list(paramlist  = "character",  # parameter names
                                   nparam     = "numeric",    # number of parameters
                                   startparam = "numeric",    # starting parameter values for estimation
                                   indices    = "list",       # a list giving the index of each parameter
                                   lhslist    = "character",  # names of dependent (i.e., stochastic) variables
                                   rhslist    = "character"   # names of independent (i.e., non-stochastic) variables
                     )
)

Model$methods(
    XBeta = function(varlist, data, param, include_constant = 0,
                     coef_prefix = "", datavar_suffix = "", constname = "constant") {
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
