df_1 <- data.frame("a" = runif(10000, 0, 1), "b" = rnorm(10000, 5, 2),
                   "c" = sample(c("x", "y", "z"), 10000, replace = T),
                   id = 1:10000)

SaveData(df_1, key = "id", outfile = "issue15/df_1.csv",
         logfile = "issue15/df1_manifest.log")

SaveData(df_1, key = "id", outfile = "issue15/df_1.RDS",
         logfile = "issue15/df1_manifest.log", appendlog = T)

SaveData(df_1, key = "id", outfile = "issue15/df_1.dta",
         logfile = "issue15/df1_manifest.log", appendlog = T)

SaveData(df_1, key = "id", outfile = "issue15/df_1.RData",
         logfile = "issue15/df1_manifest.log", appendlog = T)


