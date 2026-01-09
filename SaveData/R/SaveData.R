#' Save and summarize data
#'
#' @param df Data to be saved
#' @param key Defines key to dataset. Must be non-missing and jointly unique.
#' @param outfile Path to saved data file. Possible file extensions are .csv, .dta, .parquet, .RData, and .RDS. If no file extension is provided, the data is saved as .RDS.
#' @param logfile Path to logfile. File contains standard summary statistics for numerical variables and displays variable types for all variables. If no path is specified, the log file is saved to the same directory as the output file. Type logfile = FALSE to not output a logfile.
#' @param appendlog If TRUE, an existing log file of the same name will not be overwritten but the information appended. Default is FALSE.
#' @param sortbykey If TRUE, the data will be sorted based on the keys provided. Default is TRUE.
#'
#' @seealso \link[data.table]{fwrite}, \link[base]{save}, \link[base]{saveRDS}, \link[haven]{write_dta}, and \link[arrow]{write_parquet}
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
#' @importFrom arrow       write_parquet
#' @importFrom data.table fwrite
#' @importFrom digest     digest
#' @importFrom dplyr      arrange across all_of select distinct
#' @importFrom hash       keys hash
#' @importFrom haven      write_dta
#' @importFrom stargazer  stargazer
#' @export

SaveData <- function(df, key, outfile, logfile = NULL, appendlog = FALSE, sortbykey = TRUE) {

  # map file extension to export function
  DataDictionary <- function() {
    h <- hash::hash()
    h[["csv"]]     <-   c("fwrite", "file = outfile")
    h[["dta"]]     <-   c("write_dta", "outfile")
    h[["parquet"]] <-   c("write_parquet", "outfile")
    h[["RData"]]   <-   c("save", "file = outfile")
    h[["RDS"]]     <-   c("saveRDS", "file = outfile")
    h[["Rds"]]     <-   c("saveRDS", "file = outfile")

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

    if (!any(filetype %in% hash::keys(h))) {
      stop("FileType Error: Incorrect format. Only .csv, .dta, .parquet, .RData, and .RDS are allowed.")
    }

    if (is.null(logfile)) {
      logfile <- paste(dir, "/data_file_manifest.log", sep = "")
    }

    return(list("outfile" = outfile, "logfile" = logfile, "filetype" = filetype))
  }

  CheckColumnsNotList <- function(df) {
    column_types <- sapply(df, class)
    type_list_columns <-column_types[column_types=="list"]
    if (length(type_list_columns)>0) {
      stop(paste("TypeError: No column can contain entries of type list or vector. All columns should be in vector format. The following columns are of type list:",
                 paste(names(type_list_columns), collapse = ", ")))
    }
  }

  CheckKey <- function(df, key, colname_order = reordered_colnames) {

    missing_keys <- key[!key %in% colnames(df)]
    if (length(missing_keys) > 0) {
      stop(paste("KeyError: One or more key variables are not in df:",
                 paste(missing_keys, collapse = ", ")))
    }

    missings <- sapply(key, function(k) sum(is.na(df[[k]])))

    if (sum(missings) > 0) {
      stop(paste("KeyError: There are rows with missing keys. Check the following keys:",
                 paste(key[which(missings > 0)], collapse = ", ")))
    }

    nunique <- df %>% dplyr::distinct(across(all_of(key))) %>% nrow()

    if (nrow(df) != nunique) {

      stop("KeyError: Key variables do not uniquely identify observations.")

    }
  }


  WriteLog <- function(df, key, outfile, logfile = NULL, appendlog = TRUE) {

    reordered_colnames = names(df)

    if (logfile == FALSE) return(NULL)

    numeric_cols <- reordered_colnames[sapply(df, FUN = is.numeric)]
    non_numeric_cols <- base::setdiff(reordered_colnames, numeric_cols)

    numeric_summ <- data.frame(
      variable_name = numeric_cols,
      mean = sapply(numeric_cols, function(col) round(mean(df[[col]], na.rm = TRUE), 3)),
      sd = sapply(numeric_cols, function(col) round(sd(df[[col]], na.rm = TRUE), 3)),
      min = sapply(numeric_cols, function(col) round(min(df[[col]], na.rm = TRUE), 3)),
      max = sapply(numeric_cols, function(col) round(max(df[[col]], na.rm = TRUE), 3)),
      stringsAsFactors = FALSE
    )

    non_numeric_summ <- data.frame(
      variable_name = non_numeric_cols,
      uniqueN = sapply(non_numeric_cols, function(col) length(unique(df[[col]]))),
      stringsAsFactors = FALSE
    )

    all_summ <- data.frame(
      variable_name = reordered_colnames,
      type = sapply(df, function(x) class(x)[1]),
      N = sapply(df, function(x) sum(!is.na(x))),
      stringsAsFactors = FALSE
    )

    summ <- merge(all_summ, non_numeric_summ, by = "variable_name", all.x = TRUE)
    summ <- merge(summ, numeric_summ, by = "variable_name", all.x = TRUE)
    summ <- summ[match(reordered_colnames, summ$variable_name), ]

    hash <- digest::digest(df, algo="md5")

    if (file.exists(logfile) & appendlog) cat('\n', file = logfile, append=T)

    cat("File: ", outfile, '\n', file = logfile, append=appendlog)
    cat("MD5:  ", hash, '\n',    file = logfile, append=T)
    cat("Key:  ", key, '\n',     file = logfile, append=T)

    s = capture.output(
      stargazer::stargazer(summ,
                           summary = F,
                           type = 'text',
                           digit.separate = 3,
                           digit.separator = ',',
                           rownames = F))
    cat(paste(s,"\n"), file = logfile, append=T)

  }

  WriteData <- function(df, outfile, filetype, h) {
    if (filetype == "RData") {
      do.call(h[[filetype]][1], list("df", file = eval(parse(text=h[[filetype]][2]))))
    } else {
      do.call(h[[filetype]][1], list(df, eval(parse(text=h[[filetype]][2]))))
    }
    print(paste0("File '", outfile, "' saved successfully."))

  }

  h <- DataDictionary()
  files <- CheckExtension(outfile, h, logfile)
  CheckColumnsNotList(df)
  reordered_colnames <- c(key, setdiff(colnames(df), key))
  CheckKey(df, key, colname_order = reordered_colnames)

  if (sortbykey) {
    df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(key)))
  }

  df <- dplyr::select(df, dplyr::all_of(reordered_colnames))

  WriteLog(df, key, files$outfile, files$logfile, appendlog)
  WriteData(df, files$outfile, files$filetype, h)

}

