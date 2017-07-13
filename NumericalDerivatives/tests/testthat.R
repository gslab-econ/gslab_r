library(testthat)
library(NumericalDerivatives)

sink("tests.log")
sprintf("Tests begin at %s", Sys.time())

test_check("NumericalDerivatives")

sprintf("Tests end at %s", Sys.time())
sink()
