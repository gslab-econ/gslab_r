Model <- setRefClass(Class  = "Model",
                     # Model: A class that provides a template for models
                     
                     fields = list(paramlist  = "character",  # a vector of parameter names
                                   nparam     = "numeric",    # the number of parameters
                                   startparam = "numeric",    # a vector of starting parameter values for estimation
                                   indices    = "list",       # a list giving the index of each parameter
                                   lhslist    = "character",  # names of dependent (i.e., stochastic) variables
                                   rhslist    = "character"   # names of independent (i.e., non-stochastic) variables
                     )
)

Model$methods(
    isValidModel = function() {
        # Should be overridden in subclasses, which returns a bool to indicate whether it is a valid instantiation.
        
        return (TRUE)
    },
    
    XBeta = function(varlist, data, param, include_constant = 0,
                     coef_prefix = "", datavar_suffix = "", constname = "constant") {
        # Return X * beta
        
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
