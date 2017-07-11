constr <- MLEConstraints(paramlist = c("a", "b", "c", "d", "e", "f"))
inf <- 1e20
test_that("setUpperBound", {
    constr$setUpperBound("c", 5)
    expect_equal(constr$xU, c(1e20, inf, 5, inf, inf, inf))
    constr$setUpperBound(c("c", "d", "f"), c(1, 2, 3))
    expect_equal(constr$xU, c(inf, inf, 1, 2, inf, 3))
    expect_error(constr$setUpperBound(c("a", "b"), 5), "The dimension of the bounds does not match ")
    expect_error(constr$setUpperBound(c("g"), 5), "Names not in the list of parameters")
})

test_that("setLowerBound", {
    constr$setLowerBound("c", -5)
    expect_equal(constr$xL, c(-inf, -inf, -5, -inf, -inf, -inf))
    constr$setLowerBound(c("c", "d", "f"), c(-1, -2, -3))
    expect_equal(constr$xL, c(-inf, -inf, -1, -2, -inf, -3))
    expect_error(constr$setLowerBound(c("a", "b"), -5), "The dimension of the bounds does not match ")
    expect_error(constr$setLowerBound(c("g"), -5), "Names not in the list of parameters")
})

test_that("setFixedBound", {
    constr$setFixedBound("a", 0)
    expect_equal(constr$xU, c(0, inf, 1, 2, inf, 3))
    expect_equal(constr$xL, c(0, -inf, -1, -2, -inf, -3))
    constr$setFixedBound(c("a", "b", "c"), c(1, 2, 3))
    expect_equal(constr$xU, c(1, 2, 3, 2, inf, 3))
    expect_equal(constr$xL, c(1, 2, 3, -2, -inf, -3))
})

test_that("removeBound", {
    constr$removeBound(c("c", "f"))
    expect_equal(constr$xU, c(1, 2, inf, 2, inf, inf))
    expect_equal(constr$xL, c(1, 2, -inf, -2, -inf, -inf))
    constr$removeBound(constr$paramlist)
    expect_equal(constr$xU, c(inf, inf, inf, inf, inf, inf))
    expect_equal(constr$xL, c(-inf, -inf, -inf, -inf, -inf, -inf))
    expect_error(constr$removeBound("g"), "Names not in the list of parameters")
})

test_that("isConsistent_JacobianOfConstraints",{
    lin <- function(param) {
        a1 <- param[1]
        a2 <- 2 * param[1] + param[2] + 3 * param[3]
        return (c(a1, a2))
    }
    nonlin <- function(param) {
        a1 <- param[2] / param[1] + param[3] * param[4]^2
        a2 <- param[4] * sqrt(abs(param[1])) - param[3]
        return (c(a1, a2))
    }
    constr1 <- MLEConstraints(xL = c(-1, 2, -5, -inf), xU = c(-1, 4, 5, inf))
    expect_true(constr1$isConsistent(c(-1, 2, 0, 0)))
    expect_false(constr1$isConsistent(c(-2, 5, 6, 0)))
    expect_equal(constr1$JacobianOfConstraints(c(-1, 2, 0, 0))$xL, diag(4))
    
    constr2 <- MLEConstraints(con = lin, cL = c(-1, 3), cU = c(-1, 3))
    expect_true(constr2$isConsistent(c(-1, 2, 1, 4)))
    expect_false(constr2$isConsistent(c(2, 1, 0, 0)))
    expect_equal(constr2$JacobianOfConstraints(c(-1, 2, 1, 4))$cU[2,], c(2, 1, 3, 0), tolerance = 1e-3)
    
    constr3 <- MLEConstraints(con = lin, cU = c(-1, 3))
    expect_true(constr3$isConsistent(c(-5, 0, 1, 0)))
    expect_false(constr3$isConsistent(c(-1, 2, 4, 4)))
    expect_equal(constr3$JacobianOfConstraints(c(-5, 0, 1, 0))$cL, NULL)
    
    constr4 <- MLEConstraints(con = nonlin, cL = c(6, 1), cU = c(6, 1))
    expect_true(constr4$isConsistent(c(1, 2, 1, 2)))
    expect_false(constr4$isConsistent(c(1, 2, 1, 4)))
    expect_equal(constr4$JacobianOfConstraints(c(1, 2, 1, 2))$cL[1,], c(-2, 1, 4, 4), tolerance = 1e-3)
})
