source("ExampleData.R")

set.seed(12345)
n <- 100
x <- rnorm(n)
y <- rnorm(n)
z <- rnorm(n)
w <- rnorm(n + 1)
rhs <- list(x = x, y = y)
strvar <- sample(c("f", "m"), n, replace = TRUE)
group1 <- sort(sample(1:10, n, replace = TRUE))
group2 <- sort(sample(1:10, n - 1, replace = TRUE))
group3 <- sample(1:10, n, replace = TRUE)
arrayVar1 <- array(rnorm(3 * n), c(n, 3))
arrayVar2 <- array(rnorm(6 * n), c(n, 2, 3))

test_that("initialize", {
    a <- ExampleData(x, y, const = list(mu = 0, sigma = 1))
    expect_identical(c(a$varnames, a$nobs, a$nvars, a$const$mu), c(c("x", "y", "obsindex"), n, 3, 0))
    b <- ExampleData(rhs, z, varnames = c("rhs1", "rhs2", "rhs3"))
    expect_identical(c(b$varnames, b$nobs, b$nvars), c(c("rhs1", "rhs2", "rhs3", "obsindex"), n, 4))
    c <- ExampleData(x, strvar, stringsAsFactors = TRUE, varnames = c("x", "gender"))
    expect_identical(c(c$varnames, class(c$var$gender)), c(c("x", "gender", "obsindex"), "factor"))
    expect_error(ExampleData(x, y, varnames = c("few")), "Wrong number of variable names supplied")
})

test_that("setGroup", {
    a <- ExampleData(x, y)
    a$setGroup(group1)  # Add a new group variable
    expect_identical(a$ngroup, length(unique(group1)))
    a <- ExampleData(x, y, group1)
    a$setGroup(a$var$group1)  # Add a group variable already in the dataset
    expect_identical(a$var$group1, a$groupvar)
    expect_error(a$setGroup(group2), "The length of the group variable does not match the dataset")
    expect_error(a$setGroup(group3), "The group variable is not sorted")
})

test_that("addData", {
    a <- ExampleData(x)
    a$addData(rhs, names = c("var1", "var2"))
    expect_identical(c(a$varnames, a$nvars, ncol(a$var)), c(c("x", "obsindex", "var1", "var2"), 4, 4))
    expect_error(a$addData(x), "Duplicated variable names added")
    expect_error(a$addData(w), "arguments imply differing number of rows")
    expect_error(a$addData(x, y, names = c("var1")), "Wrong number of variable names supplied")
})

test_that("removeData", {
    a <- ExampleData(x, y, z)
    a$removeData("x")
    expect_identical(c(a$varnames, a$nvars, ncol(a$var)), c(c("y", "z", "obsindex"), 3, 3))
    a$removeData(2)
    expect_identical(c(a$varnames, a$nvars, ncol(a$var)), c(c("y", "obsindex"), 2, 2))
    expect_error(a$removeData("w"), "not in the data")
})

test_that("selectData", {
    a <- ExampleData(x, y, z, rhs, varnames = c("x", "y", "z", "x1", "y1"))
    a$selectData(col = c(1, 2, 4))
    expect_identical(c(a$varnames, a$nvars), c(c("x", "y", "x1"), 3))
    a$selectData(c(1:2, 5:8), col = c("x"))
    expect_identical(c(a$varnames, a$nobs, a$nvars), c("x", 6, 1))
    a$selectData(a$var$x > 0)
    expect_identical(a$nobs, nrow(a$var))
})

test_that("array", {
    a <- ExampleData(x)
    a$addArrayVars(arrayVar1, "array1")
    expect_identical(c(a$varnames, a$nvars, ncol(a$var), dim(a$var$array1)), 
                c(c("x", "obsindex", "array1"), 3, 3, c(n, 3, 1)))
    a$addArrayVars(arrayVar2, "array2")
    b <- a$copy()
    a$expandArrayVars()
    expect_identical(c(a$varnames[c(4, 10)], a$nvars, ncol(a$var)), 
                c(c("array1_array_2_1", "array2_array_2_2"), 11, 11))
    a$collapseArrayVars()
    expect_equal(a, b)
})

test_that("SaveLoad", {
    a <- ExampleData(x, y, z, const = list(mu = 0, sigma = 1))
    a$setGroup(group1)
    a$addArrayVars(arrayVar1, "array1")
    a$addArrayVars(arrayVar2, "array2")
    saveToDisk(a, ".", "myObj", 12)
    expect_error(saveToDisk(a, "non-existent", "myObj"), "cannot open the connection")

    b <- loadFromDisk(".", "myObj")
    expect_equal(class(a), class(b))
    expect_equal(c(a$var, a$varnames, a$groupvar, a$ngroup, a$group_size), 
                 c(b$var, b$varnames, b$groupvar, b$ngroup, b$group_size),
                 tolerance = 1e-12)
    file.remove(c("myObj.rds", "myObj.csv"))
    expect_error(loadFromDisk(".", "myObj"), "cannot open the connection")
})

test_that("misc", {
    a <- ExampleData(x, y)
    b <- a$copy()
    expect_equal(a, b)
    a$removeData("y")
    expect_lt(a$nvars, b$nvars)
    
    expect_true(a$isVariable("x"))
    expect_false(a$isVariable("w"))
})

