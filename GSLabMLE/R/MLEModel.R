#' A Reference Class that Provides a Template for Maximum Likelihood Estimation Models
#' @field group_unobs_list A vector of names of group-level unobservables (integrated numerically).
#' @field indiv_unobs_list A vector of names of individual-level unobservables (integrated numerically).
#' @field error_list: A vector of names of individual-level errors (not integrated numerically).
#' @field error_distributions A list that contains the distributions of all errors in \code{error_list}.
#' @field error_dimensions A list that contains the number of dimensions of each observations of
#' all errors in \code{error_list}.
#' @field dparamlist A vector of names of derived parameters.
#' @field ngroup_unobs The number of group-level unobservables in \code{group_unobs_list}.
#' @field nindiv_unobs The number of individual-level unobservables in \code{indiv_unobs_list}.
#' @field nerrors The number of error terms in \code{error_list}.
#' @field ndparam The number of derived parameters in \code{dparamlist}.
#' @field dindices A list that gives the index of each derived parameter.
#' @field numerical_integral An indicator of whether the model requires numerical integration.
#' @import methods GSLabModel NumericalDerivatives SparseGrid dplyr
#' @importFrom "data.table" "data.table"
#' @importClassesFrom GSLabModel Model
#' @inheritSection GSLabModel::Model Fields
#' @inheritSection GSLabModel::Model Methods
#' @export MLEModel
#' @exportClass MLEModel
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
                                    names(.self$dindices) <- .self$dparamlist
                                }
                                .self$numerical_integral  <- (.self$ngroup_unobs | .self$nindiv_unobs)
                            }
                        )
)
