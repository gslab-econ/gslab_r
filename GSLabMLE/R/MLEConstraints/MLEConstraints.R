#' A Reference Class that Defines Constraints of Parameters for a Maximum Likelihood Model.
#' @description The 
#' min f(x)
#' s.t.	cL	<=	g(x)	<=	cU
#'      xL	<=	x	    <=	xU
#' @field xL The lower bounds on the parameters.
#' @field xU The upper bounds on the parameters.
#' @filed con The constraints function.
#' @field cL The lower bounds on the constraints function.
#' @field cU The upper bounds on the constraints function.
#' @field paramlist A vector of parameter names.
#' @field nparam The number of parameters.
#' @field indices A list giving the index of each parameter.
#' @export MLEConstraints
#' @exportClass MLEConstraints
#' 
MLEConstraints <- setRefClass(Class   = "MLEConstraints",
                              fields  = list(xL        = "numeric",
                                             xU        = "numeric",
                                             con       = "function",
                                             cL        = "numeric",
                                             cU        = "numeric",
                                             paramlist = "character",
                                             nparam    = "numeric",
                                             indices   = "list"
                              ),
                              methods = list(
                                  initialize = function(xL        = numeric(0),
                                                        xU        = numeric(0),
                                                        con       = function(x) NULL,
                                                        cL        = numeric(0),
                                                        cU        = numeric(0),
                                                        paramlist = character(0)) {
                                      .self$xL        <- xL
                                      .self$xU        <- xU
                                      .self$con       <- con
                                      .self$cL        <- cL
                                      .self$cU        <- cU
                                      .self$paramlist <- paramlist
                                      .self$nparam    <- length(.self$paramlist)
                                      .self$indices   <- as.list(1:.self$nparam)
                                      names(.self$indices)   <- .self$paramlist
                                  }
                              )
)

MLEConstraints$methods(
    setUpperBound = setUpperBound,
    setLowerBound = setLowerBound,
    setFixedBound = setFixedBound,
    removeBound   = removeBound,
    constraints = function(param) {
        return (.self$con(param))
    }
)

