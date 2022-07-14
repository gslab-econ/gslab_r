test_that("correctly saves file", {
    scalar   <- 5
    filename <- "output/example_scalar.tex"

    AutoFill("SomeScalar", scalar, filename, mode = "math", appendmode = FALSE)
    expect_true(file.exists("./output/example_scalar.tex"))
})

test_that("correctly saves file when mode is text", {
    textinput     <- "sometext"
    filename      <- "output/example_text.tex"

    AutoFill("SomeScalar", textinput, filename, mode = "text", appendmode = FALSE)
    expect_true(file.exists("./output/example_text.tex"))
    
    output_file <- read.csv(filename, header = F)
    expect_true(grepl("textnormal", output_file[1]))
})

test_that("correctly appends to an existing file", {
    scalar_1   <- 2021
    scalar_2   <- 5021
    
    filename   <- "output/two_scalars.tex"

    AutoFill("Scalar_One", scalar_1, filename, mode = "math", appendmode = FALSE)
    AutoFill("Scalar_Two", scalar_2, filename, mode = "math", appendmode = TRUE)
    
    output_file <- read.csv(filename, header = F)
    expect_true(grepl("2021", output_file[1]))
    expect_true(grepl("5021", output_file[1]))
})

