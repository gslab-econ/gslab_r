MLESetOfDatasets <- setRefClass(Class  = "MLESetOfDatasets",
                                fields = list(
                                    datasets  =  "list",
                                    ndatasets = "numeric"
                                ),
                                methods = list(
                                    initialize = function(datasets = list()) {
                                        .self$datasets  <- datasets
                                        .self$ndatasets <- length(.self$datasets)
                                    }
                                )
)

MLESetOfDatasets$methods(
    addDataset = function(dataset) {
        .self$datasets[[.self$ndatasets + 1]] <- dataset
        .self$ndatasets <- length(.self$datasets)
    },
    saveDatasetsToDisk = function(directory, name, precision = 8, indices = 1:.self$ndatasets) {
        "Loops over and store data in an \\code{MleSetOfDatasets} object according to
         the \\code{saveToDisk} method of \\code{MLEData}.\n
         \\code{directory}: The location of files to be saved.
         \\code{name}: The name of files to be saved.
         \\code{precision}: The number of decimal to store for all inputs. Default is 8.
         \\code{indices}: Positional indices of the datasets to save."
        if (length(indices) == 1) {
            indices = 1:indices  
        }
        for (i in indices) {
            saveToDisk(.self$datasets[[i]], directory, paste(name, "_", i, sep = ""), precision)
        }
    },
    loadDatasetsFromDisk = function(directory, name, indices, collapseArrayVars = 1) {
        "Loops over stored data and create an \\code{MLESetOfDatasets} object according to
         the \\code{loadFromDisk} method of \\code{MLEData}.\n
         \\code{directory}: The location of files to be loaded.
         \\code{name}: The name of files to be saved.
         \\code{precision}: The number of decimal to store for all inputs. Default is 8.
         \\code{indices}: Positional indices of the datasets to save.
         \\code{collapseArrayVars}: Whether array variables are collapsed. Default is collapse."
        if (length(indices) == 1) {
            indices = 1:indices  
        }
        for (i in indices) {
            .self$datasets[[i]] = loadFromDisk(directory, paste(name, "_", i, sep = ""), collapseArrayVars)
        }
        .self$ndatasets <- length(indices)
    }
)
