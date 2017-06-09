source("ExampleModel.R")

test.ExampleModel.initialize <- function() {
    a <- ExampleModel("y", c("x1", "x2"), suffix = "_c", include_constant = 1, startparam = c(2, 2, 2))
    checkEquals(c(a$paramlist, a$lhslist, a$rhslist, a$nparam, a$include_constant, a$startparam),
                c(c("x1_c", "x2_c", "constant"), "y", c("x1","x2"), 3, 1, c(2, 2, 2)))
    b <- ExampleModel(c("y1", "y2"), "z", include_constant = 0)
    checkEquals(c(b$paramlist, b$lhslist, b$rhslist, b$nparam, b$include_constant, b$startparam),
                c("z_coeff", c("y1", "y2"), "z", 1, 0, 0))
    checkException(ExampleModel("y", "x", include_constant = 0, startparam = c(2, 2)), silent = TRUE)
}

test.ExampleModel.XBeta <- function() {
    model   <- ExampleModel("y", c("x1", "x2"))
    data    <- ModelData(x1 = rnorm(100), x2 = rnorm(100))
    varlist <- c("x1", "x2")
    param   <- c(0.2, 0.3, 1)
    checkEquals(model$XBeta(varlist, data, param, 1), 0.2 * data$var$x1 + 0.3 * data$var$x2 + 1)
}

test.ExampleModel.estimate <- function() {
    model <- ExampleModel("y", c("x1", "x2", "x3"), include_constant = 0)
    data  <- ExampleData(rnorm(100))
    est   <- model$estimate(data)
    checkEquals(c(est$nobs, est$model$nparam, est$param, est$value, est$vcov, est$se),
                c(100, 3, rep(0.5, 3), -0.75, diag(3), rep(1, 3)), tolerance = 0.0001)
}
