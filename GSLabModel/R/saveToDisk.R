#' Save a \code{ModelData} object to files
#' @description The field \code{var} is saved as \code{.csv} file and the remaining fields are saved
#' as a \code{.rds} file. The group variable is added in the \code{var} with name \code{group_export}.
#' All array variables are expanded.
#' @param obj A ModelData object.
#' @param directory The location of files to be saved.
#' @param name The name of files to be saved.
#' @param precision The number of decimal to store for all inputs. Default is 8.
#' @export
#' 
saveToDisk = function(obj, directory, name, precision = 8) {
    temp <- obj$copy()
    temp$expandArrayVars()
    if (length(temp$groupvar)) {
        temp$addData(group_export = temp$groupvar)
    }
    write.csv(round(temp$var, digits = precision), file = paste(directory, "/", name, ".csv", sep = ""), 
              row.names = FALSE)
    temp$var <- data.frame()
    saveRDS(temp, file = paste(directory, "/", name, ".rds", sep = ""))
}