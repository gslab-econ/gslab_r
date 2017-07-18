#' A Reference Class that Holds a List of Datasets
#' @field datasets A list of \code{MLEData} objects.
#' @field ndatasets The number of \code{MLEData} objects.
#' @import methods GSLabModel
#' @export MLESetOfDatasets
#' @exportClass MLESetOfDatasets
MLESetOfDatasets <- setRefClass(Class   = "MLESetOfDatasets",
                                fields  = list(datasets  = "list",
                                               ndatasets = "numeric"
                                ),
                                methods = list(
                                    initialize = function(datasets = NULL) {
                                        .self$datasets <- list()
                                        if (!is.null(datasets)) {
                                            for (i in 1:length(datasets)) {
                                                .self$datasets[[i]] <- datasets[i]
                                            }
                                        }
                                        .self$ndatasets <- length(.self$datasets)
                                    }
                                )
)

MLESetOfDatasets$methods(
    addDataset = function(dataset) {
        "\\subsection{Description}{
        Add a new \\code{MLEData} object in the list of datasets.}\n
        \\subsection{Parameters}{
        \\code{dataset}: An MLEData object to be added.}"
        .self$datasets[[.self$ndatasets + 1]] <- dataset
        .self$ndatasets <- length(.self$datasets)
    },
    
    saveDatasetsToDisk = function(directory, name, precision = 8, indices = NULL) {
        "\\subsection{Description}{
        Loops over and save datasets in an \\code{MLESetOfDatasets} object according to
        the \\code{saveToDisk} method of \\code{MLEData}.}\n
        \\subsection{Parameters}{
        \\code{directory}: The location where files will be saved. \n
        \\code{name}: The name of the files to be saved.\n
        \\code{precision}: The number of decimal places to save. Default is 8.\n
        \\code{indices}: The positional indices of the datasets to save. If not specified,
        all datasets wil be saved.\n}"
        if (is.null(indices)) {
            indices <- 1:.self$ndatasets
        }
        if (length(indices) == 1) {
            indices <- 1:indices  
        }
        for (i in indices) {
            GSLabModel::saveToDisk(.self$datasets[[i]], directory, paste(name, "_", i, sep = ""), precision)
        }
    },
    
    loadDatasetsFromDisk = function(directory, name, indices, collapseArrayVars = 1) {
        "\\subsection{Description}{
        Loops over saved datasets and create an \\code{MLESetOfDatasets} object according to
        the \\code{loadFromDisk} method of \\code{MLEData}.}\n
        \\subsection{Parameters}{
        \\code{directory}: The location where files will be loaded. \n
        \\code{name}: The name of the files to be loaded.\n
        \\code{indices}: The positional indices of the datasets to load.\n
        \\code{collapseArrayVars}: Whether array variables are collapsed. Default is collapse.}"
        if (length(indices) == 1) {
            indices <- 1:indices  
        }
        for (i in indices) {
            .self$datasets[[i]] <- GSLabModel::loadFromDisk(directory, paste(name, "_", i, sep = ""),
                                                collapseArrayVars)
        }
        .self$ndatasets <- length(indices)
    }
)
