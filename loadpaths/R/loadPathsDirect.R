#' Add paths from files to R environment
#' 
#' @param additionalPath, standardPaths
#' 
#' @return Creates string objects that hold contents of specified files, normally paths.txt
#' 
#' @seealso \code{\link{loadPaths}} to load from "^PATHS"
#' 
#' @examples
#' #Load paths in standardPaths
#' loadPathsDirect()
#' 
#' #Load paths in additionalPaths and standardPaths
#' loadPathsDirect(additionalPath = c("example/path/one.txt", "example/path/two.txt"))
#' 
#' #Load paths in additionalPaths 
#' loadPathsDirect(additionalPaths = c("example/path/one.txt", "example/path/two.txt"), 
#'                 standardPaths = "")
#' @export

loadPathsDirect <- function(additionalPaths = "",
                            standardPaths   = c("output/data/paths.txt", "lib/paths.txt")) {
    
    paths_list <- c(additionalPaths, standardPaths)
    paths_list <- paths_list[paths_list != ""]
    
    for (path in paths_list) {
      
        path_names  <- unlist(read.table(path, as.is = T, comment.char = " ")[1])
        path_values <- unlist(read.table(path, as.is = T, comment.char = " ")[2])
        
        for (i in 1:length(path_names)) {
            name  <- toString(path_names[i])
            value <- toString(path_values[i])
            assign(name, value, envir = .GlobalEnv)
        } 
    }
}