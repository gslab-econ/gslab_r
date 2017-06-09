rm(list = ls())
library(RUnit)

# init testing suite
test.suite <- defineTestSuite("gslab_model",
                              dirs = file.path("./"),
                              testFileRegexp = 'test.*.R$')
test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)