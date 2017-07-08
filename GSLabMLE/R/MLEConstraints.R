#' A Reference Class that Defines Constraints of Parameters.
#' @field lower The lower limit of the parameters.
#' @field upper The upper limit of the parameters.
#' @export MLEConstraints
#' @exportClass MLEConstraints
#' 
MLEConstraints <- setRefClass(Class   = "MLEConstraints",
                              fields  = list(lower = "numeric",
                                             upper = "numeric"
                              ),
                              methods = list(
                                  initialize = function(lower = -Inf, upper = Inf) {
                                      .self$lower <- lower
                                      .self$upper <- upper
                                  }
                              )
)
