setLowerBound <- function(.self, constr_paramlist, bounds) {
    ncparam <- length(constr_paramlist)
    inf     <- 1e20
    if (ncparam != length(bounds)) {
        stop("The dimension of the bounds does not match the dimension of the parameters")
    }
    if (any(!constr_paramlist %in% .self$paramlist)) {
        stop("Names not in the list of parameters")
    }
    if (!length(.self$xL)) {
        .self$xL <- rep(-inf, .self$nparam)
    }
    for (i in 1:ncparam) {
        .self$xL[.self$indices[[constr_paramlist[i]]]] <- bounds[i]
    }
}