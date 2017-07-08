source("ExampleModel.R")
source("../R/MLEData.R")
source("../R/MLESimulationOptions.R")
source("../R/MLESetOfDatasets.R")

set.seed(1)
n     <- 1000
mu    <- 1
sigma <- 2
param <- c(mu, sigma)
data  <- MLEData(y = rnorm(n, mu, sigma))
model <- ExampleModel("y")
replications <- 10
test_simdata <- function(simdata) {
    expect_equal(class(simdata)[1], "MLEData")
    expect_equal(simdata$varnames, c("obsindex", "epsilon", "eta", "phi", "y"))
    expect_equal(simdata$var$y, mu + simdata$var$epsilon * sigma)
}

test_that("single_simulation", {
    simopts <- MLESimulationOptions(seed = 2)
    simdata <- model$simulate(param, data, simopts)
    test_simdata(simdata)
})

test_that("multiple_simulation", {
    simopts <- MLESimulationOptions(replications = replications)
    simdata <- model$simulate(param, data, simopts)
    expect_equal(simdata$ndatasets, replications)
    for (i in 1:replications) {
        test_simdata(simdata$datasets[[i]])
    }
})
