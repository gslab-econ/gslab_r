test_that("sumWithin", {
    vin_1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 2, 4, byrow = TRUE)
    out1  <- sumWithin(vin_1, c(1, 1))
    out2  <- sumWithin(vin_1, c(2, 1))
    vin_2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 4, 2, byrow = TRUE)
    out3  <- sumWithin(vin_2, c(2, 1, 1, 1))
    out4  <- sumWithin(vin_2, c(3, 2, 1, 4))
    expect_equal(out1$group, matrix(1))
    expect_equal(out1$value, matrix(c(6, 8, 10, 12), 1))
    expect_equal(out2$group, matrix(c(1, 2), 2))
    expect_equal(out2$value, matrix(c(5, 6, 7, 8, 1, 2, 3, 4), 2, byrow = TRUE))
    expect_equal(out3$group, matrix(c(1, 2), 2))
    expect_equal(out3$value, matrix(c(15, 18, 1, 2), 2, byrow = TRUE))
    expect_equal(out4$group, matrix(c(1, 2, 3, 4), 4))
    expect_equal(out4$value, matrix(c(5, 6, 3, 4, 1, 2, 7, 8), 4, byrow = TRUE))
    
    ncol_value <- 10
    ncol_group <- 2
    ngroup     <- 3
    nrow       <- 10000
    value <- matrix(rnorm(ncol_value * nrow), nrow)
    group <- matrix(sample(ngroup, ncol_group * nrow, replace = TRUE), nrow)
    out   <- sumWithin(value, group)
    expect_equal(nrow(out$group), ngroup^ncol_group)
    expect_equal(nrow(out$value), ngroup^ncol_group)
    expect_equal(ncol(out$group), ncol_group)
    expect_equal(ncol(out$value), ncol_value)
})

test_that("prodWithin", {
    vin_1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 2, 4, byrow = TRUE)
    out1  <- prodWithin(vin_1, c(1, 1))
    out2  <- prodWithin(vin_1, c(2, 1))
    vin_2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 4, 2, byrow = TRUE)
    out3  <- prodWithin(vin_2, c(2, 1, 1, 1))
    out4  <- prodWithin(vin_2, c(3, 2, 1, 4))
    expect_equal(out1$group, matrix(1))
    expect_equal(out1$value, matrix(c(5, 12, 21, 32), 1))
    expect_equal(out2$group, matrix(c(1, 2), 2))
    expect_equal(out2$value, matrix(c(5, 6, 7, 8, 1, 2, 3, 4), 2, byrow = TRUE))
    expect_equal(out3$group, matrix(c(1, 2), 2))
    expect_equal(out3$value, matrix(c(105, 192, 1, 2), 2, byrow = TRUE))
    expect_equal(out4$group, matrix(c(1, 2, 3, 4), 4))
    expect_equal(out4$value, matrix(c(5, 6, 3, 4, 1, 2, 7, 8), 4, byrow = TRUE))
    
    ncol_value <- 10
    ncol_group <- 2
    ngroup     <- 3
    nrow       <- 10000
    value <- matrix(rnorm(ncol_value * nrow), nrow)
    group <- matrix(sample(ngroup, ncol_group * nrow, replace = TRUE), nrow)
    out   <- prodWithin(value, group)
    expect_equal(nrow(out$group), ngroup^ncol_group)
    expect_equal(nrow(out$value), ngroup^ncol_group)
    expect_equal(ncol(out$group), ncol_group)
    expect_equal(ncol(out$value), ncol_value)
})

test_that("avgWithin", {
    vin_1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 2, 4, byrow = TRUE)
    out1  <- avgWithin(vin_1, c(1, 1))
    out2  <- avgWithin(vin_1, c(2, 1))
    vin_2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 4, 2, byrow = TRUE)
    out3  <- avgWithin(vin_2, c(2, 1, 1, 1))
    out4  <- avgWithin(vin_2, c(3, 2, 1, 4))
    expect_equal(out1$group, matrix(1))
    expect_equal(out1$value, matrix(c(3, 4, 5, 6), 1))
    expect_equal(out2$group, matrix(c(1, 2), 2))
    expect_equal(out2$value, matrix(c(5, 6, 7, 8, 1, 2, 3, 4), 2, byrow = TRUE))
    expect_equal(out3$group, matrix(c(1, 2), 2))
    expect_equal(out3$value, matrix(c(5, 6, 1, 2), 2, byrow = TRUE))
    expect_equal(out4$group, matrix(c(1, 2, 3, 4), 4))
    expect_equal(out4$value, matrix(c(5, 6, 3, 4, 1, 2, 7, 8), 4, byrow = TRUE))
    
    ncol_value <- 10
    ncol_group <- 2
    ngroup     <- 3
    nrow       <- 10000
    value <- matrix(rnorm(ncol_value * nrow), nrow)
    group <- matrix(sample(ngroup, ncol_group * nrow, replace = TRUE), nrow)
    out   <- avgWithin(value, group)
    expect_equal(nrow(out$group), ngroup^ncol_group)
    expect_equal(nrow(out$value), ngroup^ncol_group)
    expect_equal(ncol(out$group), ncol_group)
    expect_equal(ncol(out$value), ncol_value)
})

test_that("expandArray", {
    numrep  <- 1e5
    x       <- matrix(1:5, 5)
    y       <- matrix(1:10, 5, byrow = TRUE)
    z       <- do.call("rbind", rep(list(y), numrep))
    
    countsx <- matrix(c(1, 1, 3, 2, 1), 5) # A matrix with one row 
    countsy <- c(1, 2, 1, 1, 2) # A vector
    countsz <- rep(countsy, numrep)
    answerx <- matrix(c(1, 2, 3, 3, 3, 4, 4, 5), 8)
    answery <- matrix(c(1, 2, 3, 4, 3, 4, 5, 6, 7, 8, 9, 10, 9, 10), 7, byrow = TRUE)
    answerz <- do.call("rbind", rep(list(answery), numrep))
    
    expect_equal(expandArray(x, countsx), answerx)
    expect_equal(expandArray(y, countsy), answery)
    expect_equal(expandArray(z, countsz), answerz)
})

test_that("groups", {
    vec <- c(1, -1, -1, 3, 1, 5)
    mat <- matrix(c(1, 2, -1, 0, -1, 0, 3, 4, 1, 2, 5, 6), 6, byrow = TRUE)
    strvec <- c("a", "b", "b", "c", "a", "d")
    strmat <- matrix(c("a", "b", "b", "c", "b", "c", "c", "d", "a", "b", "d", "e"), 6, byrow = TRUE)
    
    answer <- c(2, 1, 1, 3, 2, 4)
    stranswer <- c(1, 2, 2, 3, 1, 4)
    expect_equal(groups(vec), answer)
    expect_equal(groups(mat), answer)
    expect_equal(groups(strvec), stranswer)
    expect_equal(groups(strmat), stranswer)
})

test_that("seqWithin", {
    out1 <- seqWithin(c(1, 1))
    out2 <- seqWithin(c(2, 1))
    out3 <- seqWithin(c(2, 1, 1, 1))
    out4 <- seqWithin(c(3, 2, 1, 4))
    out5 <- seqWithin(matrix(c(2, 3, 1, 1, 3, 3, 2, 2, 3, 3, 1, 1, 3, 3), 7, byrow = TRUE))
    expect_equal(out1$indices, matrix(c(1, 2)))
    expect_equal(out1$sorted_group, matrix(c(1, 1)))
    expect_equal(out2$indices, matrix(c(1, 1)))
    expect_equal(out2$sorted_group, matrix(c(1, 2)))
    expect_equal(out3$indices, matrix(c(1, 2, 3, 1)))
    expect_equal(out3$sorted_group, matrix(c(1, 1, 1, 2)))
    expect_equal(out4$indices, matrix(c(1, 1, 1, 1)))
    expect_equal(out4$sorted_group, matrix(c(1, 2, 3, 4)))
    expect_equal(out5$indices, matrix(c(1, 2, 1, 1, 1, 2, 3)))
    expect_equal(out5$sorted_group, matrix(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3), 7, byrow = TRUE))
})