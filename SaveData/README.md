
# Overview

The SaveData package provides a function to save and summarize data.


## SaveData
Use `SaveData()` to save to standard formats and create a log file that summarizes the saved data. The log file gives standard summary statistics for numerical variables and displays variable types for all variables.

### Supported File Formats

- `.csv` - Comma-separated values (uses `data.table::fwrite`)
- `.dta` - Stata format (uses `haven::write_dta`)
- `.parquet` - Apache Parquet format (uses `arrow::write_parquet`)
- `.RData` - R data format (uses `base::save`)
- `.RDS` - R serialized format (uses `base::saveRDS`, default if no extension provided)
