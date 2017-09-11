library(testthat)
library(GSLabModel)

sink("tests.log")
sprintf("Tests begin at %s", Sys.time())

print(test_dir("testthat/"))

sprintf("Tests end at %s", Sys.time())
sink()
