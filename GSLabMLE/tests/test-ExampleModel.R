source("ExampleModel.R")

set.seed(1)
n     <- 10000
mu    <- -1
sigma <- 2
param <- c(mu, sigma)
y     <- rnorm(n, mu, sigma)
data  <- MLEData(y)
model <- ExampleModel("y")

model$computeLikelihoodByGroup(param, result$data_rep, result$nodes, result$weights)


test_that("misc", {
    expect_silent(model$isValidParameterVector(param))
})

test_that("simulate", {
    errors <- model$drawErrors(data, MLESimulationOptions())
    expect_is(errors, "list")
    expect_equal(names(errors), "epsilon")
    expect_equal(length(errors$epsilon), data$nobs)
    
    unobservables <- model$drawUnobservables(data, MLESimulationOptions())
    expect_is(unobservables, "list")
    expect_equal(names(unobservables), c("eta", "phi"))
    expect_equal(length(unobservables$eta), data$nobs)
    expect_equal(length(unobservables$phi), data$nobs)
    
    expect_equal(model$transformErrors(data, param, errors), errors)
    expect_equal(model$transformUnobservables(data, param, unobservables), unobservables)
    
    simdata <- model$simulate(param, data)
    expect_equal(simdata$varnames, c("obsindex", "eta", "phi", "epsilon", "y"))
    expect_equal(simdata$var$y, mu + simdata$var$epsilon * sigma)
})

test_that("estimate", {
    constr  <- MLEConstraints(xL = c(-1e20, 0))
    estopts <- MLEEstimationOptions(constr = constr)
    result  <- model$computeNodesAndWeights(data, estopts$quadacc)
    expect_is(result, "list")
    expect_equal(names(result), c("nodes", "weights", "data_rep"))
    expect_equal(names(result$nodes), c("values", "group", "nodenum", "obs"))
    expect_equal(names(result$nodes$values), c("eta", "phi"))
    expect_equal(names(result$weights), c("wgt", "group", "node"))
    expect_is(result$data_rep, "MLEData")
    expect_equal(length(result$nodes$values$eta), result$data_rep$nobs)
    expect_equal(length(result$nodes$values$phi), result$data_rep$nobs)
    expect_equal(length(result$nodes$group), result$data_rep$nobs)
    expect_equal(length(result$nodes$nodenum), result$data_rep$nobs)
    expect_equal(length(result$nodes$obs), result$data_rep$nobs)
    expect_equal(length(result$weights$wgt), length(result$weights$group))
    expect_equal(length(result$weights$wgt), length(result$weights$node))
    est <- model$estimate(data, estopts)
})

test_that("getDerivedParam", {
    est <- model$estimate(data)
    for (paramname in model$dparamlist) {
        dparam <- model$derivedParam(est$param, paramname)
        expect_equal(dparam, est$dparam[est$model$dindices[[paramname]]])
    }
    expect_equal(model$getDerivedParam(est$param), est$dparam)
    expect_equal(est$dparam[est$model$dindices[["lnsigma"]]],
                 log(est$param[est$model$indices[["sigma"]]]))
    expect_equal(est$dparam[est$model$dindices[["CV"]]],
                 est$param[est$model$indices[["sigma"]]] / est$param[est$model$indices[["mu"]]])
})

