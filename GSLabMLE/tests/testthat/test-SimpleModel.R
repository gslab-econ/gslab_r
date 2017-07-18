set.seed(1)
n       <- 10000
mu      <- 1
sigma   <- 2
param   <- c(mu, sigma)
data    <- MLEData(y = rep(0, n))
model   <- SimpleModel("y")
simdata <- model$simulate(param, data)
data    <- MLEData(simdata$var$y, varnames = "y")

test_that("simulate", {
    errors <- model$drawErrors(data, MLESimulationOptions())
    expect_is(errors, "list")
    expect_equal(names(errors), "epsilon")
    expect_equal(length(errors$epsilon), data$nobs)
    
    simdata <- model$simulate(param, data)
    expect_equal(simdata$varnames, c("obsindex", "epsilon", "y"))
    expect_equal(simdata$var$y, simdata$var$epsilon * param[2] + param[1])
})

test_that("estimate", {
    constr  <- MLEConstraints(xL = c(-1e20, 0))
    estopts <- MLEEstimationOptions(constr = constr)
    result  <- model$computeNodesAndWeights(data, estopts$quadacc)
    expect_equal(names(result), c("nodes", "weights", "data_rep"))

    grouplik <- model$computeLikelihoodByGroup(param, result$data_rep, result$nodes, result$weights)
    expect_equal(length(grouplik), n)
    
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
