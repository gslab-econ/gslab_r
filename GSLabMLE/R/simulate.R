simulate <- function(.self, param, data, simopts = NULL) {
    if (is.null(simopts)) {
        simopts <- MLESimulationOptions()
    }
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

singleSimulation <- function(model, param, data, simopts) {
    simdata   <- data$copy()
    if (length(model$error_list)) {
        raw_error <- model$drawErrors(simdata, simopts)
        error     <- model$transformErrors(param, simdata, raw_error)
        for (name in names(error)) {
            if (ncol(error[[name]]) > 1) {
                simdata$addArrayVars(error[[name]], name = name)
            } else {
                simdata$addData(error[[name]], names = name)
            }
        }
    }
    if (model$ngroup_unobs | model$nindiv_unobs) {
        raw_unobs <- model$drawUnobservables(simdata, simopts)
        unobs     <- model$transformUnobservables(param, simdata, raw_unobs)
        simdata$addData(unobs)
    }
    lhs <- model$computeOutcomes(param, simdata)
    for (name in names(lhs)) {
        if (name %in% simdata$varnames) {
            simdata$removeData(name)
        }
        simdata$addData(lhs[[name]], names = name)
    }
    return (simdata)
}