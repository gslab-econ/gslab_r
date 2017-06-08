ModelData <- setRefClass(Class  = "ModelData",
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
        .self$groupvar           <- value
        .self$ngroup             <- length(unique(.self$groupvar))
        .self$group_size         <- as.numeric(table(.self$groupvar))
        .self$unique_group_sizes <- unique(.self$group_size)
        .self$assertValidGroup()
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
        if (is.numeric(col))
            col <- .self$varnames[col]
        .self$varnames <- .self$varnames[!.self$varnames %in% col]
        .self$var      <- subset(.self$var, select = .self$varnames)
        .self$nvars    <- ncol(.self$var)
    },
    
    selectData = function(row = 1:nobs, col = .self$varnames) {
        data = data.frame(.self$var[row, col])
        if (is.character(col))
            colnames(data) <- col
        .self$var      <- data
        .self$varnames <- colnames(.self$var)
        .self$nobs     <- nrow(.self$var)
        .self$nvars    <- ncol(.self$var)
    },
    
    assertValidGroup = function() {
        if (length(.self$groupvar)) {
            if (!length(.self$groupvar) == .self$nobs)
                stop("Length of groupvar does not match data")
            if (!all(.self$groupvar[2:.self$nobs] - .self$groupvar[1:.self$nobs - 1] >= 0))
                stop("Group variable not sorted")
        }
    },
    
    isVariable = function(varname) {
        varname %in% .self$varnames
    },
    
    saveToDisk = function(dir, name, precision = 4) {
        obj <- .self$copy()
        if (length(obj$groupvar)) {
            obj$var <- data.frame(obj$var, group_export = obj$groupvar)
        }
        write.csv(round(obj$var, digits = precision), file = paste(dir, name, ".csv", sep = ""), row.names = FALSE)
        obj$var <- data.frame()
        base::save(obj, file = paste(dir, name, ".RData", sep = ""))
    },
    
    loadFromDisk = function(dir, name) {
        tempName <- load(paste(dir, name, ".RData", sep = ""))
        tempObj  <- get(tempName)
        for (field in names(.refClassDef@fieldClasses)) {
            .self$field(field, tempObj$field(field))
        }
        data <- read.csv(paste(dir, name, ".csv", sep = ""))
        .self$var <- data
        .self$removeData("group_export")
    }
)
