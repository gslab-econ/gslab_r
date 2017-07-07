source("ExampleModel.R")
source("../R/MLEData.R")

set.seed(1)
n     <- 10000
mu    <- 1
sigma <- 2
param <- c(mu, sigma)
y     <- rnorm(n, mu, sigma)
group <- sort(sample(10, n, replace = TRUE))
data  <- MLEData(y)
data$setGroup(group)
model <- ExampleModel("y")

expect_equal(model$transformErrors(data, param, y), y)
expect_equal(model$transformUnobservables(data, param, y), y)

simopts <- MLESimulationOptions(seed = 2)
simdata <- model$simulate(param, data, simopts)
expect_equal(simdata$varnames, c("obsindex", "epsilon", "eta", "phi", "y"))
expect_equal(simdata$var$y, mu + simdata$var$epsilon * sigma)
