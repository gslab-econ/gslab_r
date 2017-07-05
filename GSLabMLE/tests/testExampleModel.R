library("testthat")
source("ExampleModel.R")
source("../R/MLEData.R")

set.seed(1)
n     <- 10000
mu    <- 1
sigma <- 2

y <- rnorm(n, mu, sigma)
group <- sort(sample(10, n, replace = TRUE))
data  <- MLEData(y)
data$setGroup(group)
model <- ExampleModel("y")
model$estimate(data)

param <- c(mu, sigma)
expect_identical(model$transformErrors(data, param, y), y)
expect_identical(model$transformUnobservables(data, param, y), y)

simdata <- model$simulate(param, data)
expect_identical(simdata$varnames, c("obsindex", "epsilon", "eta", "nu", "y"))
expect_identical(simdata$var$y, mu + simdata$var$epsilon * sigma)
