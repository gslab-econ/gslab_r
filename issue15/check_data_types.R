remove(list=ls())
library(SaveData)

set.seed(0)

n <- 10000
df_1 <- data.frame("a" = round(runif(n, 0, 1), 2), 
                   "b" = round(rnorm(n, 5, 2), 2),
                   "c" = sample(c("x", "y", "z"), n, replace = T),
                   id = 1:n)

SaveData(df_1, key = "id", outfile = "issue15/df_1.csv",
         logfile = "issue15/df1_manifest.log")

SaveData(df_1, key = "id", outfile = "issue15/df_1.RDS",
         logfile = "issue15/df1_manifest.log", appendlog = T)

SaveData(df_1, key = "id", outfile = "issue15/df_1.dta",
         logfile = "issue15/df1_manifest.log", appendlog = T)

SaveData(df_1, key = "id", outfile = "issue15/df_1.RData",
         logfile = "issue15/df1_manifest.log", appendlog = T)