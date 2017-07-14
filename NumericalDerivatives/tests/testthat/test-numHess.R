test_that("numHess", {
    func1 <- function(x) prod(x)
    func2 <- function(x) sqrt(prod(x))
    func3 <- function(x) x[1]^2 + x[2]
    
    x1   <- c(1, 2, 3)
    x2   <- c(1, 2, 3, 4, 5)
    xTol <- 1e-4
    
    hess1 <- numHess(func1, x1, xTol)
    hess2 <- numHess(func2, x2, xTol, c(1, 3, 5), c(2, 4))
    hess3 <- numHess(func1, x1, xTol, ind_rowvar = 1)
    hess4 <- numHess(func1, x1, xTol, ind_colvar = 2)
    
    truehess1 <- matrix(c(0, 3, 2,
                          3, 0, 1,
                          2, 1, 0),
                        nrow = 3, ncol = 3, byrow = TRUE)
    truehess2 <- matrix(0.25 * c(sqrt(30), sqrt(7.5),
                                 sqrt(10/3), sqrt(5/6),
                                 sqrt(6/5), sqrt(3/10)),
                        nrow = 3, ncol = 2, byrow = TRUE)
    truehess3 <- matrix(c(0, 3, 2), nrow = 1)
    truehess4 <- matrix(c(3, 0, 1), ncol = 1)
    
    expect_is(hess1, "matrix")
    expect_is(hess2, "matrix")
    expect_is(hess3, "matrix")
    expect_is(hess4, "matrix")
    expect_equal(hess1, truehess1, tolerance = 1e-3)
    expect_equal(hess2, truehess2, tolerance = 1e-3)
    expect_equal(hess3, truehess3, tolerance = 1e-3)
    expect_equal(hess4, truehess4, tolerance = 1e-3)
    expect_error(numHess(func3, 1, xTol), "Invalid function or argument input")
    expect_error(numHess(func1, c(1, 2), xTol, c(1, 3)), "Index exceeds the length of arguments")
})
