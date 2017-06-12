source("ExampleData.R")
n <- 1000
x <- rnorm(n)
y <- rnorm(n)
z <- rnorm(n)
w <- rnorm(n + 1)
rhs <- list(x = x, y = y)
strvar <- c("f", "m", "m", "f", "m")
group1 <- sort(sample(1:10, n, replace = TRUE))
group2 <- sort(sample(1:10, n - 1, replace = TRUE))
group3 <- sample(1:10, n, replace = TRUE)
arrayVar1 <- array(rnorm(3 * n), c(n, 3))
arrayVar2 <- array(rnorm(6 * n), c(n, 2, 3))

test.ExampleData.initialize <- function() {
    a <- ExampleData(x, y, const = list(mu = 0, sigma = 1))
    checkEquals(c(a$varnames, a$nobs, a$nvars, a$const$mu), c(c("x", "y", "obsindex"), n, 3, 0))
    b <- ExampleData(rhs, z, varnames = c("rhs1", "rhs2", "rhs3"))
    checkEquals(c(b$varnames, b$nobs, b$nvars), c(c("rhs1", "rhs2", "rhs3", "obsindex"), n, 4))
    c <- ExampleData(strvar, stringsAsFactors = TRUE, varnames = "gender")
    checkEquals(c(c$varnames, class(c$var$gender)), c(c("gender", "obsindex"), "factor"))
    checkException(ExampleData(x, y, varnames = c("few")), silent = TRUE)  # Mismatched number of variable names
}

test.ExampleData.setGroup <- function() {
    a <- ExampleData(x, y)
    a$setGroup(group1)
    checkEquals(a$ngroup, length(unique(group1)))
    a <- ExampleData(x, y, group1)
    a$setGroup(a$var$group1)
    checkEquals(a$var$group1, a$groupvar)
    checkException(a$setGroup(group2), silent = TRUE)  # Length unequal to the number of observations
    checkException(a$setGroup(group3), silent = TRUE)  # Not sorted in ascending order
}

test.ExampleData.addData <- function() {
    a <- ExampleData(x)
    a$addData(y)
    checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("x", "obsindex", "y"), 3, 3))
    a$addData(rhs, names = c("var1", "var2"))
    checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("x", "obsindex", "y", "var1", "var2"), 5, 5))
    checkException(a$addData(w), silent = TRUE)  # Add a variable with unequal length
    checkException(a$addData(x, y, names = c("var1")), silent = TRUE)  # Wrong number of variable names
}

test.ExampleData.removeData <- function() {
    a <- ExampleData(x, y, z)
    a$removeData("x")
    checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("y", "z", "obsindex"), 3, 3))
    a$removeData(2)
    checkEquals(c(a$varnames, a$nvars, ncol(a$var)), c(c("y", "obsindex"), 2, 2))
    checkException(a$removeData("w"), silent = TRUE)  # Delete a non-existence variable
}

test.ExampleData.selectData <- function() {
    a <- ExampleData(x, y, z, rhs, varnames = c("x", "y", "z", "x1", "y1"))
    a$selectData(col = c(1, 2, 4))
    checkEquals(c(a$varnames, a$nvars), c(c("x", "y", "x1"), 3))
    a$selectData(c(1:2, 5:8), col = c("x"))
    checkEquals(c(a$varnames, a$nobs, a$nvars), c("x", 6, 1))
    a$selectData(a$var$x > 0)
    checkEquals(a$nobs, nrow(a$var))
}

test.ExampleData.array <- function() {
    a <- ExampleData(x)
    a$addArrayVars(arrayVar1, "array1")
    checkEquals(c(a$varnames, a$nvars, ncol(a$var), dim(a$var$array1)), 
                c(c("x", "obsindex", "array1"), 3, 3, c(n, 3, 1)))
    a$addArrayVars(arrayVar2, "array2")
    b <- a$copy()
    a$expandArrayVars()
    checkEquals(c(a$varnames[c(4, 10)], a$nvars, ncol(a$var)), 
                c(c("array1_array_2_1", "array2_array_2_2"), 11, 11))
    a$collapseArrayVars()
    checkEquals(a, b)
}

test.ExampleData.SaveLoad <- function() {
    a <- ExampleData(x, y, z, const = list(mu = 0, sigma = 1))
    a$setGroup(group1)
    a$addArrayVars(arrayVar1, "array1")
    a$addArrayVars(arrayVar2, "array2")
    a$saveToDisk(".", "myObj", 12)
    checkException(a$saveToDisk("non-existent", "myObj"), silent = TRUE)  # Non-existent directory
    
    b <- ExampleData()
    b$loadFromDisk(".", "myObj")
    checkEquals(a, b, tolerance = 1e-12)
    file.remove(c("myObj.rds", "myObj.csv"))
    checkException(b$loadFromDisk(".", "myObj"), silent = TRUE)  # Non-existent file
}

test.ExampleData.misc <- function() {
    a <- ExampleData(x, y)
    b <- a$copy()
    checkEquals(a, b)
    a$removeData("y")
    checkException(checkEquals(a, b), silent = TRUE)
    
    checkTrue(a$isVariable("x"))
    checkTrue(!a$isVariable("w"))
}
