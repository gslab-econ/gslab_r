#' Save table values in txt format for use in Tablefill. 
#'
#' @param df the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce df to a data frame.
#' @param table_name the name used both as the table tag and as the exported file’s name
#' @param outdir the directory path where the table will be saved
#' 
#' @details The function first writes a tag line in the form `<tab:table_name>` to the output file, overwriting if necessary.  
#' The table contents are then written on a new line after this tag. The file is tab-delimited and excludes
#' row and column names. All values are unquoted. 
#' 
#' @export
#' 

MakeTableTxt <- function(df, table_name, outdir){
  tag <- sprintf("<tab:%s>", table_name)
  out <- file.path(outdir, sprintf("%s.txt", table_name))
  
  write.table(tag, row.names = FALSE, quote = FALSE,
              file = out, col.names = FALSE)
  
  write.table(df, row.names = FALSE, quote = FALSE,
              file = out, append = TRUE, col.names = FALSE, sep = "\t")
}
