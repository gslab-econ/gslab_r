constraints <- function(param) {
    summation  <- sum(param[1], param[2], param[3], param[4], param[5], param[6])
    summation2 <- sum(param[1], 2*param[2], 3*param[3], 4*param[4], 5*param[5], 6*param[6])
    return(c(summation, summation2))
}

constr <- MLEConstraints(con = constraints, cL = c(0, 0), cU = c(10, 20),
                         paramlist = c("a", "b", "c", "d", "e", "f"))

test_that("setUpperBound", {
    constr$setUpperBound("c", 5)
    expect_equal(constr$xU, c(Inf, Inf, 5, Inf, Inf, Inf))
    constr$setUpperBound(c("c", "d", "f"), c(1, 2, 3))
    expect_equal(constr$xU, c(Inf, Inf, 1, 2, Inf, 3))
    expect_error(constr$setUpperBound(c("a", "b"), 5), "The dimension of the bounds does not match ")
    expect_error(constr$setUpperBound(c("g"), 5), "Names not in the list of parameters")
})

test_that("setLowerBound", {
    constr$setLowerBound("c", -5)
    expect_equal(constr$xL, c(-Inf, -Inf, -5, -Inf, -Inf, -Inf))
    constr$setLowerBound(c("c", "d", "f"), c(-1, -2, -3))
    expect_equal(constr$xL, c(-Inf, -Inf, -1, -2, -Inf, -3))
    expect_error(constr$setLowerBound(c("a", "b"), -5), "The dimension of the bounds does not match ")
    expect_error(constr$setLowerBound(c("g"), -5), "Names not in the list of parameters")
})

test_that("setFixedBound", {
    constr$setFixedBound("a", 0)
    expect_equal(constr$xU, c(0, Inf, 1, 2, Inf, 3))
    expect_equal(constr$xL, c(0, -Inf, -1, -2, -Inf, -3))
    constr$setFixedBound(c("a", "b", "c"), c(1, 2, 3))
    expect_equal(constr$xU, c(1, 2, 3, 2, Inf, 3))
    expect_equal(constr$xL, c(1, 2, 3, -2, -Inf, -3))
})

test_that("isConsistent", {
    param <- c(1, 2, 3, 4, 5, 6)
    expect_false(constr$isConsistent(param))
    param <- c(1, 2, 3, 0, 0, 0)
    expect_true(constr$isConsistent(param))
})

test_that("removeBound", {
    constr$removeBound(c("c", "f"))
    expect_equal(constr$xU, c(1, 2, Inf, 2, Inf, Inf))
    expect_equal(constr$xL, c(1, 2, -Inf, -2, -Inf, -Inf))
    constr$removeBound(constr$paramlist)
    expect_equal(constr$xU, c(Inf, Inf, Inf, Inf, Inf, Inf))
    expect_equal(constr$xL, c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf))
    expect_error(constr$removeBound("g"), "Names not in the list of parameters")
})

