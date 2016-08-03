# To be run from gslab_r/loadpaths/test

Main <- function() {
    suppressMessages(library(devtools, roxygen))
    suppressMessages(document("../"))
    suppressMessages(install("../"))
    print("PASS")

    Sys.setenv("PATHS_TEST" = "data/paths/paths.txt", "CHECKSUM_DIR" = "./data/data/")

    paths_lists <- Sys.getenv()[grep("^PATHS", names(Sys.getenv()))]

    test_data <- read.table(paths_lists[1], comment.char = " ")
    Sys.setenv(VAR1 = as.character(test_data[1, 2]))
    Sys.setenv(VAR2 = as.character(test_data[2, 2]))
    Sys.setenv(VERYVERYVERYLONGVARNAME3 = as.character(test_data[3, 2]))

    loadPaths()
    VAR1                     <- checkerror(VAR1,                     "value1")
    VAR2                     <- checkerror(VAR2,                     "veryLongValue2")
    VERYVERYVERYLONGVARNAME3 <- checkerror(VERYVERYVERYLONGVARNAME3, "value3With!@#%^^&*()")
    rm(VAR1, VAR2, VERYVERYVERYLONGVARNAME3)

    loadPathsDirect(additionalPaths = "data/paths/paths.txt", standardPaths = "")
    VAR1                     <- checkerror(VAR1,                     "value1")
    VAR2                     <- checkerror(VAR2,                     "veryLongValue2")
    VERYVERYVERYLONGVARNAME3 <- checkerror(VERYVERYVERYLONGVARNAME3, "value3With!@#%^^&*()")
    rm(VAR1, VAR2, VERYVERYVERYLONGVARNAME3)

    loadPathsDirect(additionalPaths = "", standardPaths = "data/paths/paths.txt")
    VAR1                     <- checkerror(VAR1,                     "value1")
    VAR2                     <- checkerror(VAR2,                     "veryLongValue2")
    VERYVERYVERYLONGVARNAME3 <- checkerror(VERYVERYVERYLONGVARNAME3, "value3With!@#%^^&*()")  
    rm(VAR1, VAR2, VERYVERYVERYLONGVARNAME3)

    checksum()
    print("PASS")

    print("PASS ALL")
}  

checkerror <- function(var, val) {
    stopifnot(var == val)
    print("PASS")
}  

Main()


