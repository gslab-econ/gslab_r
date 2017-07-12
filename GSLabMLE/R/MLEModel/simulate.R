simulate <- function(.self, param, data, simopts = NULL) {
    if (is.null(simopts)) {
        simopts <- MLESimulationOptions()
    }
    .self$isValidParameterVector(param)
    if (class(data) == "MLESetOfDatasets") {
        simdata <- MLESetOfDatasets()
        for (i in 1:data$ndatasets) {
            simoptsrep <- simopts
            simoptsrep$seed <- simopts$seed + i - 1
            simdata$addDataset(singleSimulation(.self, param, data$datasets[[i]], simoptsrep))
        }
    } else if (simopts$replications > 1) {
        simdata <- MLESetOfDatasets()
        for (i in 1:simopts$replications) {
            simoptsrep <- simopts
            simoptsrep$seed <- simopts$seed + i - 1
            simdata$addDataset(singleSimulation(.self, param, data, simoptsrep))
        }
    } else {
        simdata <- singleSimulation(.self, param, data, simopts)
    }
    return (simdata)
}

singleSimulation <- function(model, param, data, simopts) {
    simdata <- data$copy()
    if (model$ngroup_unobs | model$nindiv_unobs) {
        raw_unobs <- model$drawUnobservables(simdata, simopts)
        unobs     <- model$transformUnobservables(param, simdata, raw_unobs)
        simdata$addData(unobs, replace = TRUE)
    }
    if (model$nerrors) {
        raw_error <- model$drawErrors(simdata, simopts)
        error     <- model$transformErrors(param, simdata, raw_error)
        for (name in names(error)) {
            if (ncol(error[[name]]) > 1) {
                simdata$addArrayVars(error[[name]], name = name, replace = TRUE)
            } else {
                simdata$addData(error[[name]], names = name, replace = TRUE)
            }
        }
    }
    lhs <- model$computeOutcomes(param, simdata)
    simdata$addData(lhs, replace = TRUE)
    return (simdata)
}