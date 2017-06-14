#' Load a \code{ModelData} object from files saved using \code{saveToDisk} 
#' @description Data are read from a \code{.csv} file and other fields are read from a \code{.rds}
#' file. The group variable with name \code{group_export} is deleted.
#' @param obj A ModelData object, typically an empty one.
#' @param directory location of files to be loaded.
#' @param name name of files to be loaded.
#' @param collapseArrayVars whether array variables are collapsed. Default is collapse.
#' @export
#' 
loadFromDisk = function(obj, directory, name, collapseArrayVars = 1) {
    tempObj <- readRDS(paste(directory, "/", name, ".rds", sep = ""))
    for (field in names(obj$getRefClass()$fields())) {
        obj$field(field, tempObj$field(field))
    }
    data <- read.csv(paste(directory, "/", name, ".csv", sep = ""))
    obj$var <- data
    if ("group_export" %in% obj$varnames) {
        obj$removeData("group_export")
    }
    if (collapseArrayVars) {
        obj$collapseArrayVars()
    }
}