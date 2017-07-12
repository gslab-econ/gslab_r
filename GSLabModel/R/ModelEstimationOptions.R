#' A Reference Class that Defines Options for the \code{Estimate} Method of \code{Model}
#' @field outlev The output level of the Knitro optimization. A larger value indicates more verbose
#' output. The default 0 suppresses all output information.
#' @field startparam Starting paramter vector.
#' @field hesstol Numerical step for computing the Hessian.
#' @field knitrotxt A list of additional options to be passed to Knitro. See more at "help knitro".
#' @export ModelEstimationOptions
#' @exportClass ModelEstimationOptions
#' @import methods
#' 
ModelEstimationOptions <- setRefClass(Class  = "ModelEstimationOptions",
                                      fields = list(outlev     = "numeric",
                                                    startparam = "numeric",
                                                    hesstol    = "numeric",
                                                    knitrotxt  = "list"
                                      ),
                                      methods = list(
                                          initialize = function(outlev     = 0,
                                                                startparam = numeric(0),
                                                                hesstol    = 1e-4,
                                                                knitrotxt  = list()) {
                                              .self$outlev     <- outlev
                                              .self$startparam <- startparam
                                              .self$hesstol    <- hesstol
                                              .self$knitrotxt  <- knitrotxt
                                          }
                                      )
)
