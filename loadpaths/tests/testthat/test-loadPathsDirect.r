context("loadPathsDirect")    

test_that("correctly load additionalPaths", {
    loadPathsDirect(additionalPaths = "data/paths/paths.txt", standardPaths = "")
    output <- c(VAR1, VAR2, VERYVERYVERYLONGVARNAME3)

    intended_output <- c("value1", "veryLongValue2", "value3With!@#%^^&*()")

    expect_identical(output, intended_output)
})

test_that("correctly load standardPaths", {
    loadPathsDirect(additionalPaths = "", standardPaths = "data/paths/paths.txt")
    output <- c(VAR1, VAR2, VERYVERYVERYLONGVARNAME3)
    
    intended_output <- c("value1", "veryLongValue2", "value3With!@#%^^&*()")
    
    expect_identical(output, intended_output)
})
