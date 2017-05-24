source("ExampleModel.R")

test.ExampleModel.ExampleModel <- function() {
  a = ExampleModel("y", c("x1", "x2"), suffix = "_c", default_startparam = c(0, 2, 2))
  checkEquals(c(a@paramlist, a@lhslist, a@rhslist, a@nparam, a@include_constant, a@default_startparam),
              c(c("constant", "x1_c", "x2_c"), "y", c("x1","x2"), 3, 1, c(0, 2, 2)))
  b = ExampleModel(c("y1", "y2"), "z", include_constant = 0)
  checkEquals(c(b@paramlist, b@lhslist, b@rhslist, b@nparam, b@include_constant, b@default_startparam),
              c("z_coeff", c("y1", "y2"), "z", 1, 0, 0))
  checkException(ExampleModel("y", "x", include_constant = 2), silent = TRUE)
}

test.ExampleModel.estimate <- function() {
  model = ExampleModel("y", c("x1", "x2"))
  data  = ExampleData(rnorm(100))
  est   = estimate(model, data)
  checkEquals(c(est@nobs, est@model@nparam, est@param, est@vcov),
              c(100, 3, c(0.5, 0.5, 0.5), diag(3)), tolerance = 0.0001)
}
