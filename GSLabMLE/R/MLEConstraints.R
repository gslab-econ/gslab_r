#' A Reference Class that Defines Constraints for a Maximum Likelihood Model.
#' @description
#' The general optimization problem solved by optim:L-BFGS-B is:
#' \deqn{min f(x), s.t.	lower<=x<=upper}
#' When \eqn{lower=upper} the constraint becomes an equality constraint. This class defines these constraints.
#' @field lower The lower bounds on the parameters.
#' @field upper The upper bounds on the parameters.
#' @field paramlist A vector of parameter names.
#' @field nparam The number of parameters.
#' @field indices A list giving the index of each parameter.
#' @import methods
#' @export MLEConstraints
#' @exportClass MLEConstraints
MLEConstraints <- setRefClass("MLEConstraints",
                              fields  = list(lower     = "numeric",
                                             upper     = "numeric",
                                             paramlist = "character",
                                             nparam    = "numeric",
                                             indices   = "list"
                              ),
                              methods = list(
                                  initialize = function(lower     = numeric(0),
                                                        upper     = numeric(0),
                                                        cU        = numeric(0),
                                                        paramlist = character(0)) {
                                      .self$lower     <- lower
                                      .self$upper     <- upper
                                      if (length(paramlist)) {
                                          .self$paramlist <- paramlist
                                          .self$nparam    <- length(.self$paramlist)
                                          .self$indices   <- as.list(1:.self$nparam)
                                          names(.self$indices)   <- .self$paramlist
                                      }
                                      if (any(.self$lower > .self$upper)) {
                                          stop("Wrong constraints")
                                      }
                                  }
                              )
)
