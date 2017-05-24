source("ExampleData.R")
x = rnorm(100)
y = rnorm(100)
z = rnorm(100)
w = rnorm(200)

test.ExampleData.ExampleData <- function() {
  # data.frame
  data <- data.frame(x)
  a <- ExampleData(data)
  checkEquals(c(a@varnames, a@nobs, a@nvars), c(c("x"), 100, 1))
  a <- ExampleData(data, "age")
  checkEquals(c(a@varnames, a@nobs, a@nvars), c(c("age"), 100, 1))
  
  # matrix
  data <- cbind(x, y)
  b <- ExampleData(data)
  checkEquals(c(b@varnames, b@nobs, b@nvars), c(c("x", "y"), 100, 2))
  checkException(ExampleData(data, c("dup", "dup")), silent = TRUE)
  checkException(ExampleData(data, c("few")), silent = TRUE)
  checkException(ExampleData(data, c("one", "two", "more")), silent = TRUE)
  b <- ExampleData(matrix(c(2, 3, 1, 2, 3, 4), 2, 3), c("var1", "var2", "var3"))
  checkEquals(c(b@varnames, b@nobs, b@nvars), c(c("var1", "var2", "var3"), 2, 3))
  
  # vector
  c <- ExampleData(x)
  checkEquals(c(c@varnames, c@nobs, c@nvars), c(c("var1"), 100, 1))
}

test.ExampleData.addData <- function() {
  a <- ExampleData(x, "x")
  a <- addData(a, data.frame(y))
  checkEquals(c(a@varnames, a@nvars, ncol(a@var)), c(c("x", "y"), 2, 2))
  checkException(addData(a, x, "x"), silent = TRUE)
  a <- addData(a, cbind(y, z), c("var1", "var2"))
  checkEquals(c(a@varnames, a@nvars, ncol(a@var)), c(c("x", "y", "var1", "var2"), 4, 4))
  a <- addData(a, z)
  checkEquals(c(a@varnames, a@nvars, ncol(a@var)), c(c("x", "y", "var1", "var2", "var3"), 5, 5))
  checkException(addData(a, data.frame(w)), silent = TRUE)
}

test.ExampleData.removeData <- function() {
  a <- ExampleData(data.frame(x, y, z))
  a <- removeData(a, c("x", "z"))
  checkEquals(c(a@varnames, a@nvars, ncol(a@var)), c("y", 1, 1))
  checkException(removeData(a, c("y", "w")), silent = TRUE)
}

test.ExampleExampleData.selectData <- function() {
  a <- ExampleData(data.frame(x, y, z, z), varnames = c("x", "y", "z1", "z2"))
  a <- selectData(a, col = c(1, 2, 4))
  checkEquals(c(a@varnames, a@nvars), c(c("x", "y", "z2"), 3))
  a <- selectData(a, row = a@var$x > 0)
  checkEquals(a@nobs, nrow(a@var))
  a <- selectData(a, row = c(1:2, 5:8), col = c("x"))
  checkEquals(c(a@varnames, a@nobs, a@nvars), c("x", 6, 1))
}
