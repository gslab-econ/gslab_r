#' Save and summarize data
#'
#' @param df Data to be saved
#' @param key Defines key to dataset. Must be non-missing and jointly unique.
#' @param outfile Path to saved data file. Possible file extensions are .csv, .dta, RData, and .RDS. If no file extension is provided, the data is saved as .RDS.
#' @param logfile Path to logfile. File contains standard summary statistics for numerical variables and displays variable types for all variables. If no path is specified, the log file is saved to the same directory as the output file. Type logfile = FALSE to not output a logfile.
#' @param appendlog If TRUE, an existing log file of the same name will not be overwritten but the information appended. Default is FALSE.
#' @param sortbykey If TRUE, the data will be sorted based on the keys provided. Default is TRUE.
#'
#' @seealso \link[data.table]{fwrite}, \link[base]{save}, \link[base]{saveRDS}, and \link[haven]{write_dta},
#'
#' @examples
#' \dontrun{
#' #Save data to RDS
#' SaveData(data, "id", "path/output")
#'
#' #Multiple key variables
#' SaveData(data, c("country", "day"), "path/output")
#'
#' #Save to different file format
#' SaveData(data, "id", "path/output.csv")
#'
#' #Custom log file
#' SaveData(data, "id", "path/output.csv", "path/custom_logfile.log")
#' }
#'
#' @import data.table
#' @importFrom digest     digest
#' @importFrom dplyr      arrange
#' @importFrom hash       keys hash
#' @importFrom haven      write_dta
#' @importFrom stargazer  stargazer
#' @export

SaveData <- function(df, key, outfile, logfile = NULL, appendlog = FALSE, sortbykey = TRUE) {

  if (!is.data.table(df)) {
    setDT(df)
  }
  
  reordered_colnames <- c(key, setdiff(colnames(df), key))
  
  DataDictionary <- function() {
    h <- hash()
    h[["csv"]]   <-   c("fwrite", "file = outfile")
    h[["dta"]]   <-   c("write_dta", "outfile")
    h[["RData"]] <-   c("save", "file = outfile")
    h[["RDS"]]   <-   c("saveRDS", "file = outfile")
    h[["Rds"]]   <-   c("saveRDS", "file = outfile")
    
    return(h)
  }
  
  CheckExtension <- function(outfile, h, logfile = NULL) {
    
    file <- basename(outfile)
    dir  <- dirname(outfile)
    extensions <- unlist(strsplit(file, "[.]"))
    
    if (length(extensions)  > 2) {
      stop("FileNameError: Cannot have '.' in filename.")
    } else if (length(extensions) == 2) {
      filetype = extensions[[2]]
    } else {
      filetype = "RDS"
      outfile = paste(outfile, ".RDS", sep="")
    }
    
    if (!any(filetype %in% keys(h))) {
      stop("FileType Error: Incorrect format. Only .csv, .dta, .RData, and .RDS are allowed.")
    }
    
    if (is.null(logfile)) {
      logfile <- paste(dir, "/data_file_manifest.log", sep = "")
    }
    
    return(list("outfile" = outfile, "logfile" = logfile, "filetype" = filetype))
  }
  
  CheckKey <- function(df, key, colname_order = reordered_colnames) {        
    
    if (!all(key %in% colnames(df))) {
      
      stop("KeyError: One or more key variables are not in df.")
    }
    
    missings <- df[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = key]
    
    if (sum(missings) > 0) {
      stop(paste("KeyError: There are rows with missing keys. Check the following keys:", 
                 paste(key[which(missings > 0)], collapse = ", ")))
    }
    
    nunique <- uniqueN(df, key)
    
    if (nrow(df) != nunique) {
      
      stop("KeyError: Key variables do not uniquely identify observations.")
      
    } else {
      
      if (sortbykey) {
        setorderv(df, key)  # sort by key values
      }           
      
      setcolorder(df, reordered_colnames)
    }
  }
  
  
  WriteLog <- function(df, key, outfile, logfile = NULL, appendlog = TRUE,
                       colname_order = reordered_colnames) {
    
    if (logfile == FALSE) return(NULL)
    
    numeric_cols <- reordered_colnames[sapply(df, is.numeric)]
    non_numeric_cols <- setdiff(colname_order, numeric_cols)
    
    numeric_sum <- df[, .(
      variable_name = colname_order,
      mean = lapply(.SD, mean, na.rm = TRUE),
      sd = lapply(.SD, sd, na.rm = TRUE),
      min = lapply(.SD, min, na.rm = TRUE),
      max = lapply(.SD, max, na.rm = TRUE)
    ), .SDcols = numeric_cols]
    
    non_numeric_sum <- df[, .(
      variable_name = colname_order,
      uniqueN = lapply(.SD, function(x) uniqueN(x))
    ), .SDcols = non_numeric_cols]
    
    all_sum <- df[, .(
      variable_name = colname_order,
      type = sapply(.SD, class),
      N = lapply(.SD, function(x) sum(!is.na(x)))
    )]
    
    sum <- all_sum |> 
      merge(non_numeric_sum, by = "variable_name", all.x = T) |> 
      merge(numeric_sum, by = "variable_name", all.x = T)
      
    
    sum <- sum[match(colname_order, sum$variable_name),]
    
    hash <- digest(df, algo="md5")
    
    if (file.exists(logfile) & appendlog) cat('\n', file = logfile, append=T)
    
    cat("File: ", outfile, '\n', file = logfile, append=appendlog)
    cat("MD5:  ", hash, '\n',    file = logfile, append=T)
    cat("Key:  ", key, '\n',     file = logfile, append=T)
    
    s = capture.output(stargazer(sum, summary = F,type = 'text'))
    cat(paste(s,"\n"),file=logfile,append=T)
    
  }
  
  WriteData <- function(df, outfile, filetype, h) {
    
    do.call(h[[filetype]][1], list(df, eval(parse(text=h[[filetype]][2]))))
    
    print(paste0("File '", outfile, "' saved successfully."))
    
  }
  
  h <- DataDictionary()
  files <- CheckExtension(outfile, h, logfile)
  
  CheckKey(df, key)
  WriteLog(df, key, files$outfile, files$logfile, appendlog)
  WriteData(df, files$outfile, files$filetype, h)
  
}

