#' Prints a 'checksum' of datasets stored in text files
#' 
#' @description Prints a 'checksum' of datasets stored in text files which are stored in a 
#'   pipe-delimited format. It requires a shell environment variable called "CHECKSUM_DIR" which 
#'   should contain the name of the directory where the text files are stored. 
#' 
#' @return Prints a description
#' 
#' @examples
#' \dontrun{
#' checksum()
#' }
#' 
#' @export

checksum <- function () {

    dir   <- Sys.getenv("CHECKSUM_DIR")
    files <- list.files(dir)

    for (file in files) {
  
        if (grepl(".txt", file)) {
    
          cat("\n\n\n\n\n\n")
    
          table <- read.table(paste0(dir, file), sep = "|", header = T, comment.char = " ")
    
          print(Hmisc::describe(table, toupper(file), digits = 4), condense = F)
  
        }
    }
}
