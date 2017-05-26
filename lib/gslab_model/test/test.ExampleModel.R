source("exampleModel.R")

test.exampleModel.exampleModel <- function() {
  a <- exampleModel("y", c("x1", "x2"), suffix = "_c", startparams = c(2, 2, 2))
  checkEquals(c(a$paramlist, a$lhslist, a$rhslist, a$nparam, a$include_constant, a$startparam),
              c(c("constant", "x1_c", "x2_c"), "y", c("x1","x2"), 3, 1, c(2, 2, 2)))
  b <- exampleModel(c("y1", "y2"), "z", constant = 0)
  checkEquals(c(b$paramlist, b$lhslist, b$rhslist, b$nparam, b$include_constant, b$startparam),
              c("z_coeff", c("y1", "y2"), "z", 1, 0, 0))
  checkException(exampleModel("y", "x", constant = 0, startparams = c(2, 2)), silent = TRUE)
}

test.exampleModel.estimate <- function() {
  model <- exampleModel("y", "x")
  data  <- exampleData(rnorm(100))
  est   <- model$estimate(data)
  checkEquals(c(class(est), est$nobs, est$model$nparam, est$param, est$vcov, est$se),
              c("exampleEstimationOutput", 100, 2, c(0, 0), diag(2), c(1 ,1)), tolerance = 0.0001)
}
