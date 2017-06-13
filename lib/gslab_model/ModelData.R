library(stringr)
ModelData <- setRefClass(Class  = "ModelData",
                         # A ModelData object stores dataset in the field var, an R data.frame object.
                         # The constructor ModelData() accepts the same inputs as the constructor for
                         #   the R data.frame class, plus an option of specifying variable names and
                         #   an option of adding a customer list of constants characterizing the data.
                         
                         fields = list(var                = "data.frame",   # a data.frame object to hold dataset
                                       varnames           = "character",    # a vector of variable names in the dataset
                                       nvars              = "numeric",      # the number of variables in the dataset
                                       nobs               = "numeric",      # the number of observations in the dataset
                                       const              = "list",         # a list to hold constants characterizing the dataset
                                       groupvar           = "numeric",      # a vector identifying groups in the panel
                                       ngroup             = "numeric",      # the number of groups in the panel
                                       group_size         = "numeric",      # a vector of size ngroups giving the number of observations in each group
                                       unique_group_sizes = "numeric"       # unique group sizes that appear in the dataset
                         )
)

ModelData$methods(
    initialize = function(..., stringsAsFactors = FALSE,
                          varnames = NULL, const = list()) {
        data <- data.frame(..., check.names = TRUE)
        if (nrow(data) == 0) {
            return (.self)
        }
        if (!is.null(varnames)) {
            if (length(varnames) != ncol(data)) {
                stop("Wrong number of variable names supplied")
            }
            colnames(data) <- varnames
        }
        .self$var      <- data.frame(data, obsindex = 1:nrow(data))
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
        .self$nobs     <- nrow(.self$var)
        .self$const    <- const
    },
    
    setGroup = function(value) {
        # Input: a vector identifying groups
        # Add a vector identifying groups in the panel. This vector must be sorted in ascending
        #   order with length equal to the number of observations in the dataset.
        # In most cases, the group variable is also in the data. It's better to first sort the data
        #   by the group variable, then create the ModelData object and set the group variable.
        
        if (length(value) != .self$nobs) {
            stop("The length of the group variable does not match the dataset")
        }
        if (any(value[2:.self$nobs] - value[1:.self$nobs - 1] < 0)) {
            stop("The group variable is not sorted")
        }
        .self$groupvar           <- value
        .self$ngroup             <- length(unique(.self$groupvar))
        .self$group_size         <- as.numeric(table(.self$groupvar))
        .self$unique_group_sizes <- unique(.self$group_size)
    },
    
    addData = function(..., names = NULL) {
        # Input: same as the constructor of data.frame plus an option of specifying variable names
        
        newdata <- data.frame(..., stringsAsFactors = FALSE)
        if (!is.null(names)) {
            if (length(names) != ncol(newdata)) {
                stop("Wrong number of variable names supplied")
            }
            colnames(newdata) <- names
        }
        .self$var      <- data.frame(.self$var, newdata, check.names = TRUE)
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
    },
    
    removeData = function(col) {
        # Input: column index or variable names
        
        if (is.character(col)) {
            if (any(!col %in% varnames)) {
                stop(sprintf("%s not in the data", paste(c(col[!col %in% .self$varnames]), 
                                                         collapse = ", ")))
            } else {
                col <- which(.self$varnames %in% col) 
            }
        }
        .self$var      <- .self$var[-col]
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
    },
    
    selectData = function(row = 1:.self$nobs, col = .self$varnames) {
        # Input: the same as subsetting a data.frame object
        
        data <- data.frame(.self$var[row, col])
        if (is.character(col))
            colnames(data) <- col
        .self$var      <- data
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
        .self$nobs     <- nrow(.self$var)
    },
    
    isVariable = function(varname) {
        varname %in% .self$varnames
    },
    
    addArrayVars = function(data, name = "arrayvar") {
        # Input: a matrix or a 3-dimension array, added in the field var as a 3-dimension variable.
        
        d <- dim(data)
        if (is.null(d)) {
            stop("Not an array variable. Use addData instead")
        } else if (d[1] != .self$nobs) {
            stop("Length of array variable does not match data")
        } else if (length(d) > 3) {
            stop("Array structure is too complicated")
        } else if (length(d) == 2) {
            data <- array(data, c(d[1], d[2], 1))
        }
        .self$var[[name]] <- data
        .self$varnames    <- colnames(.self$var)
        .self$nvars       <- ncol(.self$var)
    },
    
    expandArrayVars = function() {
        # Expand all array variables [varname] with dimensions R by C into multiple variables 
        #   with the variable name [varname]_array_[1:R]_[1:C]
        # The reverse function of collapseArrayVars
        
        vars <- .self$varnames
        for (varname in vars) {
            d <- dim(.self$var[[varname]])
            if (!is.null(d)) {
                for (i in 1:d[2]) {
                    for (j in 1:d[3]) {
                        newname <- paste(varname, "_array_", i, "_", j, sep = "")
                        .self$addData(.self$var[[varname]][, i, j], names = newname)
                    }
                }
                .self$removeData(varname)
            }
        }
    },
    
    collapseArrayVars = function() {
        # Collapse all variables with the name [varname]_array_[d1]_[d2] to an array variable 
        #   [varname] with dimensions R = max(d1) by C = max(d2)
        # The reverse function of expandArrayVars
        
        all_arrays <- grep("_array_[0-9]+_[0-9]+$", .self$varnames, value = TRUE)  # Find names of array var
        arrayvars <- unique(sub("_array_[0-9]+_[0-9]+$", "", all_arrays))
        for (arrayvar in arrayvars) {
            # get all arrays of the array variable
            arrays <- grep(arrayvar, all_arrays, value = TRUE)
            f <- function(array) str_extract(array, "[0-9]+_[0-9]+$")
            # get all suffix of the array variable
            collection <- sapply(arrays, f)
            # get the dimension of the array variable
            maxDimension <- as.integer(strsplit(max(collection), "_")[[1]])
            # initialize an array with maxDimension
            newArray <- array(0, c(.self$nobs, maxDimension))
            for (array in arrays) {
                # extract the location of an element of the array variable
                loc <- as.integer(strsplit(str_extract(array, "[0-9]+_[0-9]+$"), "_")[[1]])
                newArray[, loc[1], loc[2]] = .self$var[[array]]
                .self$removeData(array)
            }
            .self$addArrayVars(newArray, name = arrayvar)
        }
    },
    
    saveToDisk = function(directory, name, precision = 8) {
        # Save an ModelData object to a directory. The field var is saved as .csv, 
        #   and the remaining fields are saved as .rds.
        # The group variable is added in the var with name group_export. Array variables are expanded.
        #
        # INPUTS
        #  - directory: location of files to be saved.
        #  - name: name of files to be saved.
        #  - precision: number of decimal places to store for all inputs. Default is 4.
        
        obj <- .self$copy()
        obj$expandArrayVars()
        if (length(obj$groupvar)) {
            obj$addData(group_export = obj$groupvar)
        }
        write.csv(round(obj$var, digits = precision), file = paste(directory, "/", name, ".csv", sep = ""), 
                  row.names = FALSE)
        obj$var <- data.frame()
        saveRDS(obj, file = paste(directory, "/", name, ".rds", sep = ""))
    },
    
    loadFromDisk = function(directory, name, collapseArrayVars = 1) {
        # Load files saved using SaveToDisk to an ModelData object.
        #
        # INPUTS
        #  - directory: location of files to be loaded
        #  - name: name of files to be loaded. 
        #  - collapseArrayVars: whether array variables are collapsed. Default is collapse.
        
        tempObj <- readRDS(paste(directory, "/", name, ".rds", sep = ""))
        for (field in names(.refClassDef@fieldClasses)) {
            .self$field(field, tempObj$field(field))
        }
        data <- read.csv(paste(directory, "/", name, ".csv", sep = ""))
        .self$var <- data
        .self$removeData("group_export")
        if (collapseArrayVars) {
            .self$collapseArrayVars()
        }
    }
)
