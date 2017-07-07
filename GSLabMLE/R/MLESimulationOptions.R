#' A Reference Class that defines options for the \code{simulate} method of \code{MLEModel} class.
#' @field seed The seed for the random number generator of the first simulation
#' @field replications The number of simulation replications to produce
#' @export MLESimulationOptions
#' @exportClass MLESimulationOptions
#' 
MLESimulationOptions <- setRefClass(Class   = "MLESimulationOptions ",
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