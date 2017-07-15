set.seed(1)
n      <- 10000
mu     <- -1
sigma  <- 2
ngroup <- 100
param  <- c(mu, sigma)
data   <- MLEData(y = rnorm(n, mu, sigma), group = sort(sample(ngroup, n, replace = TRUE)))
data$setGroup(data$var$group)
model  <- ExampleModel("y")
replications <- 10

test_simdata <- function(simdata) {
    expect_is(simdata, "MLEData")
    expect_equal(simdata$varnames, c("group", "obsindex", "eta", "epsilon", "y"))
    expect_equal(simdata$var$y, simdata$var$epsilon + simdata$var$eta)
}

test_that("simulation", {
    simopts      <- MLESimulationOptions()
    simopts_reps <- MLESimulationOptions(replications = replications)

    simdata      <- model$simulate(param, data, simopts)
    simdata_reps <- model$simulate(param, data, simopts_reps)
    simdata_set  <- model$simulate(param, simdata_reps, simopts)
    
    test_simdata(simdata)
    expect_equal(length(simdata_reps$datasets), replications)
    expect_equal(simdata_reps$ndatasets, replications)
    expect_equal(simdata_set$ndatasets, replications)
    for (i in 1:replications) {
        test_simdata(simdata_reps$datasets[[i]])
        test_simdata(simdata_set$datasets[[i]])
        expect_equal(simdata_reps$datasets[[i]], simdata_set$datasets[[i]])
    }
    expect_equal(simdata, simdata_reps$datasets[[1]])
    expect_false(all(simdata$var == simdata_reps$datasets[[2]]$var))
})
