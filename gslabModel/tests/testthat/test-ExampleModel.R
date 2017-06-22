source("ExampleModel.R")
source("ExampleData.R")
source("ExampleEstimationOutput.R")
set.seed(12345)

test_that("initialize", {
    a <- ExampleModel("y", c("x1", "x2"), suffix = "_c", startparam = c(0, 1, 2))
    expect_identical(c(a$paramlist, a$lhslist, a$rhslist, a$nparam, a$startparam),
                     c(c("x1_c", "x2_c", "constant"), "y", c("x1","x2"), 3, c(0, 1, 2)))
    b <- ExampleModel(c("y1", "y2"), "z", include_constant = 0)
    expect_identical(c(b$paramlist, b$lhslist, b$rhslist, b$nparam, b$include_constant, b$startparam),
                     c("z_coeff", c("y1", "y2"), "z", 1, 0, 0))
    expect_error(ExampleModel("y", "x", include_constant = 0, startparam = c(2, 2)),
                 "Incorrectly specified start parameters")
})

test_that("XBeta", {
    model <- ExampleModel("y", c("x1", "x2", "x4"))
    data  <- ModelData(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
    param <- c(0.2, 0.3, 0.4, 1)
    expect_identical(model$XBeta(c("x1", "x2"), data, param, 1),
                     0.2 * data$var$x1 + 0.3 * data$var$x2 + 1)
    expect_error(model$XBeta(c("x1", "x3"), data, param, 0), "x3_coeff is not in the model")
    expect_error(model$XBeta(c("x1", "x4"), data, param, 0), "x4 is not in the dataset")
})

test_that("estimate", {
    n     <- 10000
    x1    <- rnorm(n)
    x2    <- rnorm(n)
    y     <- 0.2 * x1 + 0.5 * x2 + 1 + rnorm(n)
    model <- ExampleModel("y", c("x1", "x2"), include_constant = 1)
    data  <- ExampleData(x1, x2, y)
    est   <- model$estimate(data)
    expect_identical(c(est$nobs, est$model$nparam, est$vcov, est$se),
                     c(n, 3, diag(3), rep(1, 3)))
    expect_equal(lm(y~x1+x2)$coeff[["x1"]], est$param[1], tolerance = 1e-3)
    expect_equal(lm(y~x1+x2)$coeff[["x2"]], est$param[2], tolerance = 1e-3)
    expect_equal(lm(y~x1+x2)$coeff[["(Intercept)"]], est$param[3], tolerance = 1e-3)
})
