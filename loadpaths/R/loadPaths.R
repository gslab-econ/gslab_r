#' Add "^PATHS" to R environment
#' 
#' @param none
#' 
#' @description Creates string objects that hold contents of environment variables containing 
#' "^PATHS" from Sys.getenv().
#' 
#' @seealso \code{\link{loadPathsDirect}} to specify path locations. 
#' 
#' @examples
#' 
#' loadPaths()
#' 
#' @export

loadPaths <- function() {
    paths_lists <- Sys.getenv()[grep("^PATHS", names(Sys.getenv()))]

    for (path in paths_lists) {
        path_names <- read.table(path, comment.char = " ")[1]
    
        for (name in levels(as.vector(path_names)[, 1])) {
            name <- toString(name)
            assign(name, Sys.getenv(name), envir = .GlobalEnv)

        }
    }
}