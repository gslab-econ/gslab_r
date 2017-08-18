#' A Reference Class that Defines Options for the \code{Estimate} Method of \code{Model}
#' @field startparam Starting paramter vector.
#' @field hesstol Numerical step for computing the Hessian.
#' @export ModelEstimationOptions
#' @exportClass ModelEstimationOptions
#' @import methods
#' 
ModelEstimationOptions <- setRefClass(Class  = "ModelEstimationOptions",
                                      fields = list(startparam = "numeric",
                                                    hesstol    = "numeric"
                                      ),
                                      methods = list(
                                          initialize = function(startparam = numeric(0),
                                                                hesstol    = 1e-4) {
                                              .self$startparam <- startparam
                                              .self$hesstol    <- hesstol
                                          }
                                      )
)
