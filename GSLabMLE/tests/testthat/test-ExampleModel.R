set.seed(1)
n      <- 5000
mu     <- 1
sigma  <- 1
ngroup <- 100
param  <- c(mu, sigma)
group  <- sort(sample(ngroup, n, replace = TRUE))
data   <- MLEData(group = group)
data$setGroup(data$var$group)
model  <- ExampleModel("y")

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
    expect_equal(names(unobservables), "eta")
    expect_equal(length(unobservables$eta), data$nobs)
    
    expect_equal(model$transformErrors(param, data, errors), errors)
    expect_equal(model$transformUnobservables(param, data, unobservables)$eta,
                 unobservables$eta * sigma + mu)
    
    simdata <- model$simulate(param, data)
    expect_equal(simdata$varnames, c("group", "obsindex", "eta", "epsilon", "y"))
    expect_equal(simdata$var$y, simdata$var$epsilon + simdata$var$eta)
})


test_that("estimate", {
    simdata <- model$simulate(param, data)
    data    <- MLEData(simdata$var$group, simdata$var$y, varnames = c("group", "y"))
    data$setGroup(data$var$group)
    
    constr  <- MLEConstraints(lower = c(-Inf, 0))
    estopts <- MLEEstimationOptions(constr = constr, quadacc = 9)
    result  <- model$computeNodesAndWeights(data, estopts$quadacc)
    expect_is(result, "list")
    expect_equal(names(result), c("nodes", "weights", "data_rep"))
    expect_equal(names(result$nodes), c("values", "group", "nodenum", "obs"))
    expect_equal(names(result$nodes$values), "eta")
    expect_equal(names(result$weights), c("wgt", "group", "node"))
    expect_is(result$data_rep, "MLEData")
    expect_equal(length(result$nodes$values$eta), result$data_rep$nobs)
    expect_equal(length(result$nodes$group), result$data_rep$nobs)
    expect_equal(length(result$nodes$nodenum), result$data_rep$nobs)
    expect_equal(length(result$nodes$obs), result$data_rep$nobs)
    expect_equal(length(result$weights$wgt), length(result$weights$group))
    expect_equal(length(result$weights$wgt), length(result$weights$node))
    
    grouplik <- model$computeLikelihoodByGroup(param, result$data_rep, result$nodes, result$weights)
    expect_equal(length(grouplik), ngroup)
    est <- model$estimate(data, estopts)
})

test_that("getDerivedParam", {
    simdata <- model$simulate(param, data)
    data    <- MLEData(simdata$var$group, simdata$var$y, varnames = c("group", "y"))
    data$setGroup(data$var$group)
    
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
