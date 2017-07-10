ExampleModel <- setRefClass(Class    = "ExampleModel",
                            contains = "Model",
                            fields   = list(include_constant = "numeric")
)

ExampleModel$methods(
    initialize = function(lhs = NULL, rhs = NULL, include_constant = 1,
                          startparam = NULL, suffix = "_coeff") {
        if (is.null(lhs))
            return (.self)
        if (include_constant) {
            .self$paramlist <- c(paste(rhs, suffix, sep = ""), "constant")
        } else {
            .self$paramlist <- paste(rhs, suffix, sep = "")
        }
        .self$nparam           <- length(.self$paramlist)
        .self$indices          <- as.list(1:.self$nparam)
        names(.self$indices)   <- .self$paramlist
        .self$lhslist          <- lhs
        .self$rhslist          <- rhs
        .self$include_constant <- include_constant
        if (!is.null(startparam)) {
            .self$startparam <- startparam
        } else {
            .self$startparam <- rep(0, .self$nparam)
        }
        .self$isValidModel()
    },
    
    isValidModel = function() {
        if (length(.self$startparam) != .self$nparam) {
            stop("Incorrectly specified start parameters")
        } else {
            return (TRUE)
        }
    },
    
    estimate = function(data, estopts, coeff_suffix = "_coeff") {
        if (!length(estopts$startparam)) {
            estopts$startparam <- .self$startparam
        }
        # Estimate a linear regression
        f <- function(param) {
            xbeta <- .self$XBeta(.self$rhslist, data, param, .self$include_constant, coeff_suffix = "_coeff")
            L2 <- sum((data$var[[.self$lhslist]] - xbeta)^2)
            return (L2)
        }
        slvr <- knitro(x0 = estopts$startparam, objective = f)
        est  <- ExampleEstimationOutput(slvr, .self, data, estopts)
        return (est)
    }
)
