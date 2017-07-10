setFixedBound <- function(.self, constr_paramlist, bounds) {
    .self$setUpperBound(constr_paramlist, bounds)
    .self$setLowerBound(constr_paramlist, bounds)
}