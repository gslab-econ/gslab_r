isConsistent <- function(.self, param, tolerance = 1e-4) {
    is_consistent <- 1
    if (length(.self$xL)) {
        is_consistent <- is_consistent & all(param >= .self$xL)
    }
    if (length(.self$xU)) {
        is_consistent <- is_consistent & all(param <= .self$xU)
    }
    if (length(.self$cL)) {
        is_consistent <- is_consistent & all(.self$con(param) >= .self$cL)
    }
    if (length(.self$cU)) {
        is_consistent <- is_consistent & all(.self$con(param) <= .self$cU)
    }
    return (is_consistent)
}