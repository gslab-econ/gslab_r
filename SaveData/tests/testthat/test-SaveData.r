context("SaveData")
library(arrow)
library(data.table)
library(tibble)
library(dplyr)

test_that("correctly saves data", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    if (file.exists("./output/logfile.log")) file.remove("./output/logfile.log")
    output <- SaveData(test_data,"id","./output/data", "./output/logfile.log")

    intended_output <- "File './output/data.RDS' saved successfully."

    expect_identical(output, intended_output)
})

test_that("correctly saves data to .RData", {
  test_data <- read.csv("./data/data.csv", header = TRUE)
  
  if (file.exists("./output/logfile.log")) file.remove("./output/logfile.log")
  output <- SaveData(test_data,"id","./output/data.RData", "./output/logfile.log")
  
  intended_output <- "File './output/data.RData' saved successfully."
  
  expect_identical(output, intended_output)
})

test_that("correctly saves data when working with data.table", {
  test_data <- fread("./data/data.csv", header = TRUE)

  if (file.exists("./output/logfile.log")) file.remove("./output/logfile.log")
  output <- SaveData(test_data,"id","./output/data", "./output/logfile.log")

  intended_output <- "File './output/data.RDS' saved successfully."

  expect_identical(output, intended_output)
})

test_that("correctly saves data to different format", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    if (file.exists("./output/logfile.log")) file.remove("./output/logfile.log")

    output <- SaveData(test_data,"id","./output/data.csv", "./output/logfile.log")

    intended_output <- "File './output/data.csv' saved successfully."

    expect_identical(output, intended_output)

})

test_that("correctly saves data to parquet format", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    if (file.exists("./output/logfile.log")) file.remove("./output/logfile.log")

    output <- SaveData(test_data,"id","./output/data.parquet", "./output/logfile.log")

    intended_output <- "File './output/data.parquet' saved successfully."

    expect_identical(output, intended_output)
})

test_that("correctly saves data.table to parquet format", {
    test_data <- fread("./data/data.csv", header = TRUE)

    if (file.exists("./output/logfile.log")) file.remove("./output/logfile.log")

    output <- SaveData(test_data,"id","./output/data.parquet", "./output/logfile.log")

    intended_output <- "File './output/data.parquet' saved successfully."

    expect_identical(output, intended_output)
})


test_that("correctly saves data without logfile specified", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    if (file.exists("./output/data_file_manifest.log")) file.remove("./output/data_file_manifest.log")

    output <- SaveData(test_data,"id","./output/data")

    intended_output <- "File './output/data.RDS' saved successfully."

    expect_identical(output, intended_output)

    expect_true(file.exists("./output/data_file_manifest.log"))

})

test_that("correctly saves data without logfile", {
  test_data <- read.csv("./data/data.csv", header = TRUE)

  output <- SaveData(test_data,"id","./output/data", logfile = FALSE)

  intended_output <- "File './output/data.RDS' saved successfully."

  expect_identical(output, intended_output)
})

test_that("appends to logfile", {

  test_data <- read.csv("./data/data.csv", header = TRUE)

  output <- SaveData(test_data,"id","./output/data", appendlog = TRUE)

  intended_output <- "File './output/data.RDS' saved successfully."

  expect_identical(output, intended_output)
})

test_that("does not append to logfile", {

  test_data <- read.csv("./data/data.csv", header = TRUE)

  output <- SaveData(test_data,"id","./output/data", appendlog = FALSE)

  intended_output <- "File './output/data.RDS' saved successfully."

  expect_identical(output, intended_output)
})

test_that("correctly saves data with different key", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    output <- SaveData(test_data,c("partid1","partid2"),"./output/data", "./output/logfile.log")

    intended_output <- "File './output/data.RDS' saved successfully."

    expect_identical(output, intended_output)
})

test_that("correctly saves data without sorting", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    output <- SaveData(test_data,c("partid1","partid2"),"./output/data", "./output/logfile.log",
                        sortbykey = FALSE)

    intended_output <- "File './output/data.RDS' saved successfully."

    expect_identical(output, intended_output)
})

test_that("correctly gives error for nonexistent key", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    expect_error(SaveData(test_data,"wrongkey","./output/data", "./output/logfile.log"),
                 "KeyError: One or more key variables are not in df.")
})

test_that("correctly gives error for column of type list", {
  test_data <- read.csv("./data/data.csv", header = TRUE)
  test_data$list <- lapply(1:nrow(test_data), function (x) c(1, 2))
  expect_error(SaveData(test_data,c("partid1","partid2"),"./output/data", "./output/logfile.log"),
               "TypeError: No column can contain entries of type list or vector. All columns should be in vector format.")
})


test_that("correctly gives error for nonunique key", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    expect_error(SaveData(test_data,"name","./output/data", "./output/logfile.log"),
                 NULL)
})

test_that("correctly gives error for missing key", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    expect_error(SaveData(test_data,"num","./output/data", "./output/logfile.log"),
                 NULL)
})

test_that("correctly gives error for wrong data format", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    expect_error(SaveData(test_data,"id","./output/data.xlsx", "./output/logfile.log"),
                 NULL)
})


test_that("correctly gives error for wrong filename", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

    expect_error(SaveData(test_data,"id","./output/data.1.RDS", "./output/logfile.log"),
                 NULL)
})

test_that("preserves classes", {
    tibble_data <- tibble::as_tibble(read.csv("./data/data.csv", header = TRUE))
    expect_true(inherits(tibble_data, "tbl_df"))

    SaveData(tibble_data, "id", "./output/data.RDS", logfile = FALSE)
    reloaded_tibble <- readRDS("./output/data.RDS")
    expect_true(inherits(reloaded_tibble, "tbl_df"))

    dt_data <- data.table::as.data.table(read.csv("./data/data.csv", header = TRUE))
    expect_true(inherits(dt_data, "data.table"))

    SaveData(dt_data, "id", "./output/data.RDS", logfile = FALSE)
    reloaded_dt <- readRDS("./output/data.RDS")
    expect_true(inherits(reloaded_dt, "data.table"))
})

test_that("saving does not mutate shared columns among datasets", {
  dt_test <- data.frame(
    year       = c(3, 2, 1),
    rmse       = c(3, 2, 1),
    model_vars = "model vars",
    stringsAsFactors = FALSE
  )
  
  out1 <- dt_test %>% dplyr::select(-rmse)
  out2 <- dt_test %>% dplyr::select(year, rmse)
  
  expected_out2_in_memory <- as.data.frame(out2)
  
  out1_path <- "./output/shared_out1.csv"
  out2_path <- "./output/shared_out2.csv"
  if (file.exists(out1_path)) file.remove(out1_path)
  if (file.exists(out2_path)) file.remove(out2_path)
  
  SaveData(out1,
           key = "year",
           outfile = out1_path,
           logfile = FALSE,
           appendlog = TRUE)
  
  expect_identical(out2, expected_out2_in_memory)
  
  SaveData(out2,
           key = "year",
           outfile = out2_path,
           logfile = FALSE,
           appendlog = TRUE)
})

