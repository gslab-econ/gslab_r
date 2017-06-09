ModelData <- setRefClass(Class  = "ModelData",
                         # A ModelData object stores data in the field var, an R data.frame object.
                         # The constructor ModelData() accepts the same inputs as the 
                         #   constructor for the R data.frame class.
                         # See /test/ for examples 
                         
                         fields = list(var                = "data.frame",   # data.frame object to hold actual data variables
                                       nobs               = "numeric",      # number of observations
                                       varnames           = "character",    # list of names of variables in the dataset 
                                       nvars              = "numeric",      # number of observations
                                       const              = "list",         # list to hold constants characterizing the dataset
                                       groupvar           = "numeric",      # variable identifying groups in panel
                                       ngroup             = "numeric",      # number of groups in panel
                                       group_size         = "numeric",      # vector of size ngroups x 1 giving number of observations in each group
                                       unique_group_sizes = "numeric"       # unique group sizes that appear in the data
                         ),
                         methods = list(
                             initialize = function(..., varnames = NULL, const = list()) {
                                 data <- data.frame(..., check.names = TRUE)
                                 if (nrow(data) == 0)
                                     return (.self)
                                 if (!is.null(varnames)) {
                                     if (length(varnames) != ncol(data))
                                         stop("Wrong number of variable names supplied")
                                     colnames(data) <- varnames
                                 }
                                 .self$var      <- cbind(data, obsindex = 1:nrow(data))
                                 .self$nobs     <- nrow(.self$var)
                                 .self$varnames <- colnames(.self$var)
                                 .self$nvars    <- ncol(.self$var)
                                 .self$const    <- const
                             }
                         )
)

ModelData$methods(
    setGroup = function(value) {
        # Add a vector identifying groups in the panel. This vector must be sorted in
        #   ascending order with length equal to the number of observations.
        
        if (length(value) != .self$nobs)
            stop("Length of group variable does not match data")
        if (any(value[2:.self$nobs] - value[1:.self$nobs - 1] < 0))
            stop("Group variable not sorted")
        .self$groupvar           <- value
        .self$ngroup             <- length(unique(.self$groupvar))
        .self$group_size         <- as.numeric(table(.self$groupvar))
        .self$unique_group_sizes <- unique(.self$group_size)
    },
    
    addData = function(..., names = NULL) {
        newdata <- data.frame(...)
        if (nrow(newdata) != .self$nobs) 
            stop("Number of observations is mismatched.")
        if (!is.null(names)) {
            if (length(names) != ncol(newdata))
                stop("Wrong number of variable names supplied")
            colnames(newdata) = names
        }
        .self$var      <- data.frame(.self$var, newdata, check.names = TRUE)
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
    },
    
    removeData = function(col) {
        if (is.character(col))
            col <- which(.self$varnames %in% col)
        .self$var      <- .self$var[-col]
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
    },
    
    selectData = function(row = 1:.self$nobs, col = .self$varnames) {
        data <- data.frame(.self$var[row, col])
        if (is.character(col))
            colnames(data) <- col
        .self$var      <- data
        .self$varnames <- colnames(.self$var)
        .self$nobs     <- nrow(.self$var)
        .self$nvars    <- ncol(.self$var)
    },
    
    isVariable = function(varname) {
        varname %in% .self$varnames
    },
    
    addArrayVars = function(data, name = "arrayvar") {
        # Add a matrix or a 3-dimension array as a 3-dimension variable.
        
        d <- dim(data)
        if (is.null(d))
            stop("Not an array variable. Use addData instead")
        if (d[1] != .self$nobs)
            stop("Length of array variable does not match data")
        if (length(d) > 3)
            stop("Array structure is too complicated")
        if (length(d) == 2)
            data <- array(data, c(d[1], d[2], 1))
        .self$var[[name]] <- data
        .self$nvars    <- ncol(.self$var)
        .self$varnames <- colnames(.self$var)
    },
    
    expandArrayVars = function() {
        # Expand all array variables [varname] with dimensions R by C into multiple variables 
        #   with the form [varname]_array_[1:R]_[1:C]
        # The reverse function of collapseArrayVars
        
        vars <- .self$varnames
        for (varname in vars) {
            d <- dim(.self$var[[varname]])
            if (!is.null(d)) {
                for (i in 1:d[2]) {
                    for (j in 1:d[3]) {
                        name <- paste(varname, "_array_", i, "_", j, sep = "")
                        .self$addData(.self$var[[varname]][, i, j], names = name)
                    }
                }
                .self$removeData(varname)
            }
        }
    },
    
    collapseArrayVars = function() {
        # Collapse all variables of the form [varname]_array_[d1]_[d2] to an array variable 
        #   [varname] with dimensions R = max(d1) by C = max(d2)
        # The reverse function of expandArrayVars
        
        arrays <- grep("_array_[0-9]+_[0-9]+$", .self$varnames, value = TRUE)
        arrayvars <- unique(sub("_array_[0-9]+_[0-9]+$", "", arrays))
        for (arrayvar in arrayvars) {
            f = function (array) sub(paste(arrayvar, "_array_", sep = ""),  "", array)
            collection <- lapply(grep(arrayvar, arrays, value = TRUE), f)
            maxDimension <- as.integer(unlist(strsplit(max(unlist(collection)), "_")))
            newArray <- array(0, c(.self$nobs, maxDimension))
            for (array in grep(arrayvar, arrays, value = TRUE)) {
                loc <- as.integer(unlist(strsplit(sub(paste(arrayvar, "_array_", sep = ""), 
                                                      "", array), "_")))
                newArray[, loc[1], loc[2]] = .self$var[[array]]
                .self$removeData(array)
            }
            .self$addArrayVars(newArray, name = arrayvar)
        }
    },
    
    saveToDisk = function(directory, name, precision = 4) {
        # Saves an ModelData object to a directory. The field var is saved as a CSV, 
        #   and the remaining fields are saved as .RData.
        # The group variable is added in var. Array variables are expanded.
        #
        # INPUTS
        #  - directory: location of files to be saved.
        #  - name: name of files to be saved.
        #  - precision: number of decimal places to store for all inputs. Default is 4.
        
        obj <- .self$copy()
        obj$expandArrayVars()
        if (length(obj$groupvar))
            obj$addData(group_export = obj$groupvar)
        write.csv(round(obj$var, digits = precision), file = paste(directory, name, ".csv", sep = ""), 
                  row.names = FALSE)
        obj$var <- data.frame()
        base::save(obj, file = paste(directory, name, ".RData", sep = ""))
    },
    
    loadFromDisk = function(dir, name, collapseArrayVars = 1) {
        # Loads ModelData structure saved using SaveToDisk to an ModelData object.
        #
        # INPUTS
        #  - directory: location of files to be loaded
        #  - name: name of files to be loaded. 
        #  - collapseArrayVars: whether array variables are collapsed. Default is collapse.
        
        tempName <- load(paste(dir, name, ".RData", sep = ""))
        tempObj  <- get(tempName)
        for (field in names(.refClassDef@fieldClasses))
            .self$field(field, tempObj$field(field))
        data <- read.csv(paste(dir, name, ".csv", sep = ""))
        .self$var <- data
        .self$removeData("group_export")
        if (collapseArrayVars)
            .self$collapseArrayVars()
    }
)
