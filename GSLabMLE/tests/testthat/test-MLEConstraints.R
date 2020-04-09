constr <- MLEConstraints(paramlist = c("a", "b", "c", "d", "e", "f"))
test_that("setUpperBound", {
    constr$setUpperBound("c", 5)
    expect_equal(constr$upper, c(Inf, Inf, 5, Inf, Inf, Inf))
    constr$setUpperBound(c("c", "d", "f"), c(1, 2, 3))
    expect_equal(constr$upper, c(Inf, Inf, 1, 2, Inf, 3))
    expect_error(constr$setUpperBound(c("a", "b"), 5), "The dimension of the bounds does not match ")
    expect_error(constr$setUpperBound(c("g"), 5), "Names not in the list of parameters")
})

test_that("setLowerBound", {
    constr$setLowerBound("c", -5)
    expect_equal(constr$lower, c(-Inf, -Inf, -5, -Inf, -Inf, -Inf))
    constr$setLowerBound(c("c", "d", "f"), c(-1, -2, -3))
    expect_equal(constr$lower, c(-Inf, -Inf, -1, -2, -Inf, -3))
    expect_error(constr$setLowerBound(c("a", "b"), -5), "The dimension of the bounds does not match ")
    expect_error(constr$setLowerBound(c("g"), -5), "Names not in the list of parameters")
})

test_that("setFixedBound", {
    constr$setFixedBound("a", 0)
    expect_equal(constr$upper, c(0, Inf, 1, 2, Inf, 3))
    expect_equal(constr$lower, c(0, -Inf, -1, -2, -Inf, -3))
    constr$setFixedBound(c("a", "b", "c"), c(1, 2, 3))
    expect_equal(constr$upper, c(1, 2, 3, 2, Inf, 3))
    expect_equal(constr$lower, c(1, 2, 3, -2, -Inf, -3))
})

test_that("removeBound", {
    constr$removeBound(c("c", "f"))
    expect_equal(constr$upper, c(1, 2, Inf, 2, Inf, Inf))
    expect_equal(constr$lower, c(1, 2, -Inf, -2, -Inf, -Inf))
    constr$removeBound(constr$paramlist)
    expect_equal(constr$upper, c(Inf, Inf, Inf, Inf, Inf, Inf))
    expect_equal(constr$lower, c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf))
    expect_error(constr$removeBound("g"), "Names not in the list of parameters")
})

test_that("isConsistent_jacobianOfConstraints",{
    constr <- MLEConstraints(lower = c(-1, 2, -5, -Inf), upper = c(-1, 4, 5, Inf))
    expect_true(constr$isConsistent(c(-1, 2, 0, 0)))
    expect_false(constr$isConsistent(c(-2, 5, 6, 0)))
    expect_equal(constr$jacobianOfConstraints(c(-1, 2, 0, 0))$lower, diag(4))
})
