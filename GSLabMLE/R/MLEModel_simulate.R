#' @include MLEModel.R
MLEModel$methods(
    simulate = function(param, data, simopts = NULL) {
        "\\subsection{Description}{
          Simulates data from an \\code{MLEModel}.}\n
          \\subsection{Parameters}{
          \\code{param}: A vector of parameters at which to simulate data.\n
          \\code{data}: An \\code{MLEData} or \\code{MLESetOfDatasets} object.\n
          \\code{simopts}: An \\code{MLESimulationOptions} object. If not specified, the default
          simulation options will be used.}\n
          \\subsection{Return}{
          If \\code{simopts.replications} equal to 1 and \\code{data} is not an 
          \\code{MLESetOfDatasets} object: An \\code{MLEData} object which include data input, as
          well as errors, unobservables and dependent variables from random draws.\n
          If \\code{simopts.replications} is larger than 1 or \\code{data} is an 
          \\code{MLESetOfDatasets} object: An \\code{MLESetOfDatasets} object with a list of 
          \\code{MLEData} objects. Each element includes data input, as well as errors,
          unobservables and dependent variables from random draws using different seed. The seed is
          incremented between each replication.}"
        if (is.null(simopts)) {
            simopts <- MLESimulationOptions()
        }
        .self$isValidParameterVector(param)
        if (class(data) == "MLESetOfDatasets") {
            simdata <- MLESetOfDatasets()
            for (i in 1:data$ndatasets) {
                simoptsrep <- simopts$copy()
                simoptsrep$seed <- simopts$seed + i - 1
                simdata$addDataset(GSLabMLE:::singleSimulation(.self, param, data$datasets[[i]], simoptsrep))
            }
        } else if (simopts$replications > 1) {
            simdata <- MLESetOfDatasets()
            for (i in 1:simopts$replications) {
                simoptsrep <- simopts$copy()
                simoptsrep$seed <- simopts$seed + i - 1
                simdata$addDataset(GSLabMLE:::singleSimulation(.self, param, data, simoptsrep))
            }
        } else {
            simdata <- GSLabMLE:::singleSimulation(.self, param, data, simopts)
        }
        return (simdata)
    }
)

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