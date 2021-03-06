#' Load a \code{ModelData} object from files saved using \code{saveToDisk} 
#' @description Data are read from a \code{.csv} file and other fields are read from a \code{.rds}
#' file. The group variable with name \code{group_export} is deleted.
#' @param directory The location of files to be loaded.
#' @param name The name of files to be loaded.
#' @param collapseArrayVars Whether array variables are collapsed. Default is collapse.
#' @export
#' 
loadFromDisk = function(directory, name, collapseArrayVars = 1) {
    obj <- readRDS(paste(directory, "/", name, ".rds", sep = ""))
    data <- read.csv(paste(directory, "/", name, ".csv", sep = ""))
    obj$var <- data
    if ("group_export" %in% obj$varnames) {
        obj$removeData("group_export")
    }
    if (collapseArrayVars) {
        obj$collapseArrayVars()
    }
    return (obj)
}