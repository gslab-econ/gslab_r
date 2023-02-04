#' Load scalars from a text file into global environment.
#'
#' @param input path to `.txt` file.
#' @param convert whether to guess data type. Defaults to `TRUE`. If `FALSE`, stores values as strings.
#' @param ... additional arguments to `utils::type.convert`.
#' 
#' @details Each line of the input file is assumed to have the name of the global variable followed by 
#' a space followed by the value of the global variable. Lines beginning with # are treated as comments. 
#' Lines in the file that do not have at least two words (defined as blocks of text separated by a 
#' space) are ignored. Strings enclosed in double quotes count as one word.
#' 
#' @importFrom stringr str_split
#' @importFrom utils type.convert
#' @export
#' 

LoadGlobals <- function(input, convert = TRUE, ...) {
  
  stopifnot("input must be a `.txt` file" = grepl("\\.txt$", tolower(input)))
  con <- file(input, "r")
  where <- 1
  
  while (TRUE) {
    
    line = readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    
    line_vec <- stringr::str_split(trimws(line), " ", n = 2)[[1]]
    if (!startsWith(line_vec[1], "#") & length(line_vec) == 2) {
      
      name <- make.names(line_vec[1])
      if (name != line_vec[1]) {
        
        warning(sprintf("`%s' is an invalid name, skipping", line_vec[1]))
        
      } else if (exists(name, where = as.environment(where))) {
        
        warning(sprintf("`%s' already exists, skipping", line_vec[1]))
        
        } else {
        
        value <- scan(text = line_vec[2], what = "character", quiet = TRUE)
        if (convert) {
          
          if ("as.is" %in% names(list(...))) {
            value <- utils::type.convert(value, ...)
            
          } else {
            value <- utils::type.convert(value, as.is = TRUE, ...)
          }
        }
        
        assign(name, value, envir = as.environment(where))
      }
    }
  }
  close(con)
}
