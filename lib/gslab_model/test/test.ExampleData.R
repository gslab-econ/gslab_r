source("exampleData.R")
x = rnorm(100)
y = rnorm(100)
z = rnorm(100)
w = rnorm(200)

test.exampleData.initialize <- function() {
  a <- exampleData(data.frame(x))
  checkEquals(c(a$varnames, a$nobs, a$nvars), c(c("x"), 100, 1))
  data <- cbind(y, x)
  b <- exampleData(data, c("lhs", "rhs"))
  checkEquals(c(b$varnames, b$nobs, b$nvars), c(c("lhs", "rhs"), 100, 2))
  checkException(exampleData(data, c("dup", "dup")), silent = TRUE)
  checkException(exampleData(data, c("few")), silent = TRUE)
  checkException(exampleData(data, c("one", "two", "more")), silent = TRUE)
  c <- exampleData(matrix(c(2, 3, 1, 2, 3, 4), 2, 3))
  checkEquals(c(c$varnames, c$nobs, c$nvars), c(c("var1", "var2", "var3"), 2, 3))
  d <- exampleData(x)
  checkEquals(c(d$varnames, d$nobs, d$nvars), c(c("var1"), 100, 1))
}

test.exampleData.addData <- function() {
  a <- exampleData(x, "x")
  a$addData(data.frame(y))
  checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("x", "y"), 2, 2))
  checkException(a$addData(x, "x"), silent = TRUE)
  a$addData(cbind(y, z), c("var1", "var2"))
  checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("x", "y", "var1", "var2"), 4, 4))
  a$addData(z)
  checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("x", "y", "var1", "var2", "var3"), 5, 5))
  checkException(a$addData(data.frame(w)), silent = TRUE)
}

test.exampleData.removeData <- function() {
  a <- exampleData(data.frame(x, y, z))
  b <- a$copy()
  b$removeData(c("x", "z"))
  checkEquals(c(b$varnames, b$nvars, ncol(b$var)), c("y", 1, 1))
  a$removeData(1)
  checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("y", "z"), 2, 2))
  checkException(a$removeData(c("y", "w")), silent = TRUE)
}

test.ExampleexampleData.selectData <- function() {
  a <- exampleData(data.frame(x, y, z, z), varnames = c("x", "y", "z1", "z2"))
  b <- a$copy()
  a$selectData()
  checkEquals(b, a)
  a$selectData(col = c(1, 2, 4))
  checkEquals(c(a$varnames, a$nvars), c(c("x", "y", "z2"), 3))
  a$selectData(row = c(1:2, 5:8), col = c("x"))
  checkEquals(c(a$varnames, a$nobs, a$nvars), c("x", 6, 1))
  a$selectData(row = a$var$x > 0)
  checkEquals(a$nobs, nrow(a$var))
}

