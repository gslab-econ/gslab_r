removeBound <- function(.self, constr_paramlist) {
    ncparam <- length(constr_paramlist)
    inf     <- 1e20
    .self$setUpperBound(constr_paramlist, rep(inf, ncparam))
    .self$setLowerBound(constr_paramlist, rep(-inf, ncparam))
}