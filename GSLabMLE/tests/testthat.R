library(testthat)
library(GSLabMLE)
source("testthat/SimpleModel.R")
source("testthat/ExampleModel.R")

sink("tests.log")
sprintf("Tests begin at %s", Sys.time())

print(test_dir("testthat/"))

sprintf("Tests end at %s", Sys.time())
sink()
