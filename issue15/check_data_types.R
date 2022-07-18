remove(list=ls())
#library(SaveData)
#devtools::load_all()
#devtools::test()
#devtools::document()
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

df_soc <- read.csv('issue15/soc_scores.csv')

SaveData(df_soc, key = "soc", outfile = "issue15/df_soc.csv",
         logfile = "issue15/df_soc_manifest.log")

SaveData(df_soc, key = "soc", outfile = "issue15/df_soc.RDS",
         logfile = "issue15/df_soc_manifest.log", appendlog = T)

SaveData(df_soc, key = "soc", outfile = "issue15/df_soc.dta",
         logfile = "issue15/df_soc_manifest.log", appendlog = T)

df_scatters <- read.csv('issue15/scatters_data.csv')

SaveData(df_scatters, key = "ssyk96", outfile = "issue15/df_scatters.csv",
         logfile = "issue15/df_scatters_manifest.log")

SaveData(df_scatters, key = "ssyk96", outfile = "issue15/df_scatters.RDS",
         logfile = "issue15/df_scatters_manifest.log", appendlog = T)

SaveData(df_scatters, key = "ssyk96", outfile = "issue15/df_scatters.dta",
         logfile = "issue15/df_scatters_manifest.log", appendlog = T)

