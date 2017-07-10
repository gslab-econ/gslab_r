setUpperBound <- function(.self, constr_paramlist, bounds) {
    ncparam <- length(constr_paramlist)
    if (ncparam != length(bounds)) {
        stop("The dimension of the bounds does not match the dimension of the parameters")
    }
    if (any(!constr_paramlist %in% .self$paramlist)) {
        stop("Names not in the list of parameters")
    }
    if (!length(.self$cU)) {
        .self$cU <- rep(Inf, .self$nparam)
    }
    for (i in 1:ncparam) {
        .self$cU[.self$indices[[constr_paramlist[i]]]] <- bounds[i]
    }
}