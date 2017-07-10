removeBound <- function(.self, constr_paramlist) {
    ncparam <- length(constr_paramlist)
    .self$setUpperBound(constr_paramlist, rep(Inf, ncparam))
    .self$setLowerBound(constr_paramlist, rep(-Inf, ncparam))
}