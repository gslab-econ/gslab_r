#' A Reference Class that Defines Options for the \code{simulate} Method of \code{MLEModel} Class
#' @field seed The seed of the random number generator for the first simulation.
#' @field replications The number of simulation replications to produce.
#' @import methods GSLabModel
#' @export MLESimulationOptions
#' @exportClass MLESimulationOptions
MLESimulationOptions <- setRefClass(Class   = "MLESimulationOptions",
                                    fields  = list(seed         = "numeric",
                                                   replications = "numeric"
                                    ),
                                    methods = list(
                                        initialize = function(seed         = 0,
                                                              replications = 1) {
                                            .self$seed         <- seed
                                            .self$replications <- replications
                                        }
                                    )
)