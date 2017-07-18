#' A Reference Class that Defines Constraints for a Maximum Likelihood Model.
#' @description The format of the constraints follows the format expected by the R interface to
#' Knitro. See \code{\link[KnitroR]{knitro}} for details.
#' 
#' The general optimization problem solved is:
#' \deqn{min f(x), s.t.	cL<=g(x)<=cU, xL<=x<=xU}
#' where \eqn{g(x)} can be linear or non-linear. When \eqn{cL=cU} or \eqn{xL=xU}, the constraint
#' becomes an euqality constraint. This class defines these constraints. Note that Knitro solver
#' does not accept \eqn{-Inf} or \eqn{Inf} as a constraint. Instead, set a small or large enough
#' constraint, say, 1e20.
#' @field xL The lower bounds on the parameters.
#' @field xU The upper bounds on the parameters.
#' @field con The constraints function.
#' @field cL The lower bounds on the constraint functions.
#' @field cU The upper bounds on the constraint functions.
#' @field paramlist A vector of parameter names.
#' @field nparam The number of parameters.
#' @field indices A list giving the index of each parameter.
#' @field jacob_tol Default numerical precision for evaluating Jacobian.
#' @import methods
#' @export MLEConstraints
#' @exportClass MLEConstraints
MLEConstraints <- setRefClass("MLEConstraints",
                              fields  = list(xL        = "numeric",
                                             xU        = "numeric",
                                             con       = "function",
                                             cL        = "numeric",
                                             cU        = "numeric",
                                             paramlist = "character",
                                             nparam    = "numeric",
                                             indices   = "list",
                                             jacob_tol = "numeric"
                              ),
                              methods = list(
                                  initialize = function(xL        = numeric(0),
                                                        xU        = numeric(0),
                                                        con       = function(x) NULL,
                                                        cL        = numeric(0),
                                                        cU        = numeric(0),
                                                        paramlist = character(0),
                                                        jacob_tol = 1e-4) {
                                      .self$xL        <- xL
                                      .self$xU        <- xU
                                      .self$con       <- con
                                      .self$cL        <- cL
                                      .self$cU        <- cU
                                      .self$paramlist <- paramlist
                                      .self$nparam    <- length(.self$paramlist)
                                      .self$indices   <- as.list(1:.self$nparam)
                                      names(.self$indices)   <- .self$paramlist
                                      .self$jacob_tol <- jacob_tol
                                  }
                              )
)
