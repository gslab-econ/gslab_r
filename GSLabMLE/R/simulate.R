simulate <- function(.self, param, data) {
    simdata <- singleSimulation(.self, param, data)
    return (simdata)
}

singleSimulation <- function(.self, param, data) {
    simdata   <- data$copy()
    if (length(.self$error_list)) {
        raw_error <- .self$drawErrors(simdata)
        error     <- .self$transformErrors(param, simdata, raw_error)
        simdata$addData(error)
    }
    if (.self$ngroup_unobs | .self$nindiv_unobs) {
        raw_unobs <- .self$drawUnobservables(simdata)
        unobs     <- .self$transformUnobservables(param, simdata, raw_unobs)
        simdata$addData(unobs)
    }
    lhs <- .self$computeOutcomes(param, simdata)
    simdata$removeData(.self$lhslist)
    simdata$addData(lhs)
    return (simdata)
}