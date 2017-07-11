test_that("numJacob", {
    test1 <- function(x) {
        a <- x[1]^2 / x[2]
        return (a)
    }
    test2 <- function(x) {
        a1 <- x^2
        a2 <- x
        return (c(a1, a2))
    }
    test3 <- function(x) {
        a1 <- x[1]^2 / x[2]
        a2 <- x[1] + 2 * x[2]^3
        return (c(a1, a2))
    }
    test4 <- function(x) {
        a1 <- x[1, 1]^2 / x[1, 2]
        a2 <- x[2, 1] + 2 * x[2, 2]^3
        return (c(a1, a2))
    }
    x1   <- c(2, 3)
    x2   <- 2
    x3   <- c(2, 3)
    x4   <- matrix(c(2, 2, 3, 3), c(2, 2))
    xTol <- 1e-6 
    
    Jacob1 <- numJacob(test1, x1, xTol) # 1 * 2
    Jacob2 <- numJacob(test2, x2, xTol) # 2 * 1
    Jacob3 <- numJacob(test3, x3, xTol) # 2 * 2
    Jacob4 <- numJacob(test4, x4, xTol) # 2 * 4
    
    trueJacob1 <- c(4/3, -4/9)
    trueJacob2 <- matrix(c(4, 1), 2, 1)
    trueJacob3 <- matrix(c(4/3, -4/9, 1, 54), 2, 2, byrow = TRUE)
    trueJacob4 <- matrix(c(4/3, 0, -4/9, 0, 0, 1, 0, 54), 2, 4, byrow = TRUE)
    
    expect_equal(Jacob1, trueJacob1, tolerance = 1e-3)
    expect_equal(Jacob2, trueJacob2, tolerance = 1e-3)
    expect_equal(Jacob3, trueJacob3, tolerance = 1e-3)
    expect_equal(Jacob4, trueJacob4, tolerance = 1e-3)
    
})
