library(data.table)
library(SaveData)
library(tidyverse)

rm(list = ls())
dt_test <- data.frame(
  year = c(3:1),
  rmse = c(3:1),
  model_vars = "model vars"
)

out1 <- dt_test %>% dplyr::select(-c(rmse))
out2 <- dt_test %>% dplyr::select(year, rmse)
print(out2)
SaveData(out1 %>% dplyr::select(-model_vars),
         key = "year",
         outfile = "issue/out1.csv",
         logfile = F,
         appendlog = T)
print(out2)
SaveData(out2, key = "year",
         outfile = "issue/out2.csv",
         logfile = F,
         appendlog = T)
