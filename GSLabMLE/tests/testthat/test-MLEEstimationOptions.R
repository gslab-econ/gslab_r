set.seed(1)
test_that("MLEEstimationOptions", {
    data     <- MLEData(y = rnorm(10000, 1, 2))
    model    <- SimpleModel("y")
    constr   <- MLEConstraints(lower = c(2, 0))
    estopts1 <- MLEEstimationOptions(constr = constr)
    estopts2 <- MLEEstimationOptions(compute_hessian = 0)
    estopts3 <- MLEEstimationOptions(compute_jacobian = 0)
    
    est1     <- model$estimate(data, estopts1)
    est2     <- model$estimate(data, estopts2)
    est3     <- model$estimate(data, estopts3)

    expect_equal(est2$param, est3$param)
    expect_equal(est1$param[1], 2)
    expect_false(length(est2$hessian) | length(est3$jacobian))
    expect_true(length(est2$jacobian) & length(est3$hessian))
})
