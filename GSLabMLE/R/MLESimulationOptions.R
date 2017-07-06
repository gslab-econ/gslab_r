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