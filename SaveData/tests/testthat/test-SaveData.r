context("SaveData")

test_that("correctly saves data", {
    test_data <- read.csv("./data/data.csv", header = TRUE)

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

