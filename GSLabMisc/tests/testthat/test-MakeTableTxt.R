test_that("correctly saves file", {
  df       <- read.csv("input/costco_prices.csv")
  filename <- "output/costco_prices.txt"
  
  MakeTableTxt(df, "costco_prices", "output")
  
  expect_true(file.exists(filename))
})

test_that("correctly writes table tag", {
  df       <- read.csv("input/costco_prices.csv")
  filename <- "output/costco_prices.txt"
  
  MakeTableTxt(df, "costco_prices", "output")
  lines <- readLines(filename)
  
  expect_true(lines[1] == "<tab:costco_prices>")
})

test_that("correctly writes table contents", {
  df       <- read.csv("input/costco_prices.csv")
  filename <- "output/costco_prices.txt"
  
  MakeTableTxt(df, "costco_prices", "output")
  lines <- readLines(filename)
  
  expected_lines <- apply(df, 1, function(row) paste(row, collapse = "\t"))
  actual_lines <- lines[-1] 
  expect_true(all(expected_lines == actual_lines))
})

test_that("correctly overwrites existing file", {
  df       <- read.csv("input/costco_prices.csv")
  df_row1 <- df[1,]
  filename <- "output/costco_prices.txt"
  
  MakeTableTxt(df, "costco_prices", "output")
  MakeTableTxt(df_row1, "costco_prices", "output")
  lines <- readLines(filename)
  
  expected_lines <- apply(df_row1, 1, function(row) paste(row, collapse = "\t"))
  actual_lines <- lines[-1] 
  expect_true(all(expected_lines == actual_lines))
})
