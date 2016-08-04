context("loadPaths")

test_that("correctly reads data", {
    Sys.setenv("PATHS_TEST" = "./data/paths/paths.txt")
    paths_lists <- Sys.getenv()[grep("^PATHS", names(Sys.getenv()))]

    test_data <- read.table(paths_lists[1], comment.char = " ")
    Sys.setenv(VAR1 = as.character(test_data[1, 2]))
    Sys.setenv(VAR2 = as.character(test_data[2, 2]))
    Sys.setenv(VERYVERYVERYLONGVARNAME3 = as.character(test_data[3, 2]))

    loadPaths()
    output <- c(VAR1, VAR2, VERYVERYVERYLONGVARNAME3)

    intended_output <- c("value1", "veryLongValue2", "value3With!@#%^^&*()")
    
    expect_identical(output, intended_output)
})
