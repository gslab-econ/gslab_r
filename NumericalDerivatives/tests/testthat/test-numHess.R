test_that("numHess", {
    func1 <- function(x) prod(x)
    func2 <- function(x) sqrt(prod(x))
    
    x1   <- c(1, 2, 3)
    x2   <- c(1, 2, 3, 4, 5)
    xTol <- 1e-4
    
    hess1 <- numHess(func1, x1, xTol)
    hess2 <- numHess(func2, x2, xTol, c(1, 3, 5), c(2, 4))
    
    truehess1 <- matrix(c(0, 3, 2,
                          3, 0, 1,
                          2, 1, 0),
                        nrow = 3, ncol = 3, byrow = TRUE)
    truehess2 <- matrix(0.25 * c(sqrt(30), sqrt(7.5),
                                 sqrt(10/3), sqrt(5/6),
                                 sqrt(6/5), sqrt(3/10)),
                        nrow = 3, ncol = 2, byrow = TRUE)
    
    expect_equal(hess1, truehess1, tolerance = 1e-3)
    expect_equal(hess2, truehess2, tolerance = 1e-3)
})
