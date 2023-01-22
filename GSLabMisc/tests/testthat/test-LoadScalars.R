test_that("breaks if invalid file type", {
  
    filename <- "output/example_scalar.tex"
    expect_error(LoadScalars(filename))
})

test_that("correctly loads numerals", {
    
    filename     <- "input/txt/random.txt"
    expect_warning(LoadScalars(filename), "`f' already exists")
    
    expect_true(class(a) == "numeric")
    expect_true(class(b) == "numeric")
    expect_true(class(d) == "numeric")
    expect_true(class(e) == "numeric")
    expect_true(class(f) == "numeric")
    
    expect_true(class(c) == "function")
    expect_true(f == -0.8204684)
})

test_that("raises warnings", {
  
  filename     <- "input/txt/diverse.txt"
  expect_warning(LoadScalars(filename), "`*' is an invalid name")
  
  expect_true(class(y) == "character")
  expect_true(class(v) == "logical")
  expect_true(class(x) == "integer")
  expect_true(length(z) == 1)
  expect_true(length(w) == 1)
  expect_true(length(y) == 6)
  expect_true(is.na(u))
  
})
