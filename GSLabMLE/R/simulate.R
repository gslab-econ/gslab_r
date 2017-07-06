simulate <- function(.self, param, data, simopts = MLESimulationOptions()) {
    if (simopts$replications > 1) {
        simdata <- MLESetOfDatasets()
        simopts$seed <- simopts$seed - 1
        for (i in 1:simopts$replications) {
            simopts$seed <- simopts$seed + 1
            simdata$addDataset(singleSimulation(.self, param, data, simopts))
        }
    } else {
        simdata <- singleSimulation(.self, param, data, simopts)
    }
    return (simdata)
}

singleSimulation <- function(.self, param, data, simopts) {
    simdata   <- data$copy()
    if (length(model$error_list)) {
        raw_error <- model$drawErrors(simdata, simopts)
        error     <- model$transformErrors(param, simdata, raw_error)
        simdata$addData(error)
    }
    if (model$ngroup_unobs | model$nindiv_unobs) {
        raw_unobs <- model$drawUnobservables(simdata, simopts)
        unobs     <- model$transformUnobservables(param, simdata, raw_unobs)
        simdata$addData(unobs)
    }
    lhs <- model$computeOutcomes(param, simdata)
    simdata$removeData(model$lhslist)
    simdata$addData(lhs)
    return (simdata)
}