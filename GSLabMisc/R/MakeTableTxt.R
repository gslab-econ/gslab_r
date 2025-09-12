#' Load scalars from a text file into global environment.
#'
#' @param df the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce df to a data frame.
#' @param table_name 
#' @param outdir
#' 
#' @details Each line of the input file is assumed to have the name of the global variable followed by 
#' a space followed by the value of the global variable. Lines beginning with # are treated as comments. 
#' Lines in the file that do not have at least two words (defined as blocks of text separated by a 
#' space) are ignored. Strings enclosed in double quotes count as one word.
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