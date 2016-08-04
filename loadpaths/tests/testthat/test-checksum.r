context("checksum")

test_that("check checksum output", {

    Sys.setenv("CHECKSUM_DIR" = "./data/data/")

    intended_output <- paste0("\n\n\n\n\n\nDATA.TXT \n\n 2  Variables      3  Observations\n", 
                              "----------------------------------------", 
                              "----------------------------------------\n", 
                              "VAR \n      n missing  unique \n      3       0       3 \n\n",           
                              "          VAR1 VAR2 VERYVERYVERYLONGVARNAME3\nFrequency    1    1",                         
                              "                        1\n%           33   33                       33\n", 
                              "----------------------------------------", 
                              "----------------------------------------\n",
                              "VAL \n      n missing  unique \n      3       0       3 \n\n", 
                              "          value1 value3With!@#%^^&*() veryLongValue2\n", 
                              "Frequency      1                    1              1\n", 
                              "%             33                   33             33\n", 
                              "----------------------------------------", 
                              "----------------------------------------")

    expect_output(checksum(), intended_output, fixed = TRUE)
})
