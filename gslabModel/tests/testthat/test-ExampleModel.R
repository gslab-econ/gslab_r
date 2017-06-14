source("ExampleModel.R")
source("ExampleData.R")
source("ExampleEstimationOutput.R")

test_that("initialize", {
    a <- ExampleModel("y", c("x1", "x2"), suffix = "_c", startparam = c(0, 1, 2))
    expect_equal(c(a$paramlist, a$lhslist, a$rhslist, a$nparam, a$startparam),
                 c(c("x1_c", "x2_c", "constant"), "y", c("x1","x2"), 3, c(0, 1, 2)))
    b <- ExampleModel(c("y1", "y2"), "z", include_constant = 0)
    expect_equal(c(b$paramlist, b$lhslist, b$rhslist, b$nparam, b$include_constant, b$startparam),
                 c("z_coeff", c("y1", "y2"), "z", 1, 0, 0))
    expect_error(ExampleModel("y", "x", include_constant = 0, startparam = c(2, 2)),
                 "Incorrectly specified start parameters")
})

test.ExampleModel.XBeta <- function() {
    model   <- ExampleModel("y", c("x1", "x2"))
    data    <- ModelData(x1 = rnorm(100), x2 = rnorm(100))
    varlist <- c("x1", "x2")
    param   <- c(0.2, 0.3, 1)
    expect_equal(model$XBeta(varlist, data, param, 1),
                 0.2 * data$var$x1 + 0.3 * data$var$x2 + 1)
}

test.ExampleModel.estimate <- function() {
    model <- ExampleModel("y", c("x1", "x2", "x3"), include_constant = 0)
    data  <- ExampleData(x = rnorm(100))
    est   <- model$estimate(data)
    expect_equal(c(est$nobs, est$model$nparam, est$param, est$value, est$vcov, est$se),
                 c(100, 3, rep(0, 3), 0, diag(3), rep(1, 3)), tolerance = 0.0001)
}
