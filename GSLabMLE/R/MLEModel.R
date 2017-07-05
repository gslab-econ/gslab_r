library(GSLabModel)
source("../R/estimate.R")
source("../R/simulate.R")
source("../R/drawErrors.R")
source("../R/drawUnobservables.R")
source("../R/transformErrors.R")
source("../R/transformUnobservables.R")

MLEModel <- setRefClass(Class    = "MLEModel",
                        contains = "Model",
                        fields   = list(group_unobs_list    = "character",
                                        indiv_unobs_list    = "character",
                                        error_list          = "character",
                                        error_distributions = "list",
                                        error_dimensions    = "list",
                                        dparamlist          = "character",
                                        ngroup_unobs        = "numeric",
                                        nindiv_unobs        = "numeric",
                                        nerrors             = "numeric",
                                        ndparam             = "numeric",
                                        dindices            = "list",
                                        numerical_integral  = "logical"),
                        methods = list(
                            initialize = function(paramlist,
                                                  startparam          = rep(0, length(paramlist)),
                                                  lhslist             = as.character(NULL),
                                                  rhslist             = as.character(NULL),
                                                  group_unobs_list    = as.character(NULL),
                                                  indiv_unobs_list    = as.character(NULL),
                                                  error_list          = as.character(NULL),
                                                  error_distributions = as.list(NULL),
                                                  error_dimensions    = as.list(NULL),
                                                  dparamlist          = as.character(NULL)) {
                                if (is.null(paramlist)) {
                                    return (.self)
                                }
                                .self$paramlist           <- paramlist
                                .self$startparam          <- startparam
                                .self$lhslist             <- lhslist
                                .self$rhslist             <- rhslist
                                .self$group_unobs_list    <- group_unobs_list
                                .self$indiv_unobs_list    <- indiv_unobs_list
                                .self$error_list          <- error_list
                                .self$error_distributions <- error_distributions
                                .self$error_dimensions    <- error_dimensions
                                .self$dparamlist          <- dparamlist
                                # Dependent fields
                                .self$nparam              <- length(.self$paramlist)
                                .self$indices             <- as.list(1:.self$nparam)
                                names(.self$indices)      <- .self$paramlist
                                .self$ngroup_unobs        <- length(.self$group_unobs_list)
                                .self$nindiv_unobs        <- length(.self$indiv_unobs_list)
                                .self$nerrors             <- length(.self$error_list)
                                .self$ndparam             <- length(.self$dparamlist)
                                if (.self$ndparam != 0) {
                                    .self$dindices        <- as.list(1:.self$ndparam)
                                    names(.self$dindices)  <- .self$dparamlist
                                }
                                .self$numerical_integral  <- (.self$ngroup_unobs | .self$nindiv_unobs)
                            }
                        )
)

MLEModel$methods(
    estimate = estimate,
    transformErrors = transformErrors,
    transformUnobservables = transformUnobservables,
    drawErrors = drawErrors,
    drawUnobservables = drawUnobservables,
    simulate = simulate
)
