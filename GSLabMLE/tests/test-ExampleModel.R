source("ExampleModel.R")

set.seed(1)
n     <- 10000
mu    <- 1
sigma <- 2
param <- c(mu, sigma)
y     <- rnorm(n, mu, sigma)
data  <- MLEData(y)
model <- ExampleModel("y")

expect_silent(model$isValidParameterVector(param))
expect_equal(model$transformErrors(data, param, y), y)
expect_equal(model$transformUnobservables(data, param, y), y)

simdata <- model$simulate(param, data, MLESimulationOptions())
expect_equal(simdata$varnames, c("obsindex", "epsilon", "eta", "phi", "y"))
expect_equal(simdata$var$y, mu + simdata$var$epsilon * sigma)

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
