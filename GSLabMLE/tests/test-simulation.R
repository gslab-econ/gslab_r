library("testthat")
source("ExampleModel.R")
source("../R/MLEData.R")

set.seed(1)
n     <- 10000
mu    <- 1
sigma <- 2
param <- c(mu, sigma)
y     <- rnorm(n, mu, sigma)
data  <- MLEData(y)
model <- ExampleModel("y")
replications <- 10

simopts <- MLESimulationOptions(seed = 2)
simdata <- model$simulate(param, data, simopts)
expect_equal(simdata$varnames, c("obsindex", "epsilon", "eta", "phi", "y"))
expect_equal(simdata$var$y, mu + simdata$var$epsilon * sigma)
expect_equal(class(simdata)[1], "MLEData")

simopts <- MLESimulationOptions(replications = replications)
simdata <- model$simulate(param, data, simopts)
expect_equal(simdata$ndatasets, replications)
expect_equal(simdata$datasets[[1]]$varnames, c("obsindex", "epsilon", "eta", "phi", "y"))

