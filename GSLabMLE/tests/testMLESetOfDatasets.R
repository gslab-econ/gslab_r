library("testthat")
source("../R/MLEData.R")
source("../R/MLESimulationOptions.R")
source("../R/MLESetOfDatasets.R")
source("ExampleModel.R")

set.seed(1)
n <- 100
replications <- 10
simopts <- MLESimulationOptions(replications = replications)
group   <- floor(((1 : n) + 1) / 2)
data    <- MLEData(y = rnorm(n, 1, 2))
data$setGroup(group)
model   <- ExampleModel("y")
simdata <- model$simulate(c(1, 2), data, simopts)
expect_equal(simdata$ndatasets, replications)
expect_equal(length(simdata$datasets), replications)
expect_equal(class(simdata$datasets[[1]])[1], "MLEData")

simdata$saveDatasetsToDisk(".", "test", 12)

simdata_load <- MLESetOfDatasets();
simdata_load$loadDatasetsFromDisk(".", "test", replications)
expect_equal(class(simdata), class(simdata_load))
expect_equal(simdata$ndatasets, simdata_load$ndatasets)
for (i in 1:replications) {
    a <- simdata$datasets[[i]]
    b <- simdata_load$datasets[[i]]
    expect_equal(class(a), class(b))
    expect_equal(c(a$var, a$varnames, a$nvars, a$nobs, a$groupvar, a$ngroup, a$group_size), 
                 c(b$var, b$varnames, b$nvars, b$nobs, b$groupvar, b$ngroup, b$group_size),
                 tolerance = 1e-12)
    file.remove(c(sprintf("test_%s.rds", i), sprintf("test_%s.csv", i)))
}