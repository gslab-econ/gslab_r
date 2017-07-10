library(testthat)
library(GSLabModel)
library(methods)
library(KnitroR)

sink("tests.log")
sprintf("Tests begin at %s", Sys.time())

test_check("GSLabModel")

sprintf("Tests end at %s", Sys.time())
sink()