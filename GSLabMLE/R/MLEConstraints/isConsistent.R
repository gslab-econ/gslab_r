isConsistent <- function(.self, param, tolerance = 1e-4) {
    is_consistent = 1
    if (length(constr$xL)) {
        is_consistent = is_consistent & all(param >= constr$xL)
    }
    if (length(constr$xU)) {
        is_consistent = is_consistent & all(param <= constr$xU)
    }
    if (length(constr$cL)) {
        is_consistent = is_consistent & all(constr$con(param) >= constr$cL)
    }
    if (length(constr$cU)) {
        is_consistent = is_consistent & all(constr$con(param) <= constr$cU)
    }
    return (is_consistent)
}