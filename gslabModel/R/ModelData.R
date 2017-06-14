#' A Reference Class that Provides a Template for Data
#' @description A ModelData object stores dataset in the field var, an R data.frame object.
#' The constructor \code{ModelData()} accepts the same inputs as the constructor for a data frame,
#' plus an option of specifying variable names and an option of adding a customer list of
#' constants characterizing the data.
#' @examples
#' x <- rnorm(5)
#' y <- rnorm(5)
#' rhs <- list(x = x, y = y)
#' strvar <- c("f", "m", "m", "f", "m")
#' ModelData(x, y, const = list(mu = 0, sigma = 1))
#' ModelData(rhs, strvar, varnames = c("rhs1", "rhs2", "gender"))
#' @field var A data.frame object to hold dataset
#' @field varnames A vector of variable names in the dataset
#' @field nvars The number of variables in the dataset
#' @field nobs The number of observations in the dataset
#' @field const A list to hold constants characterizing the dataset
#' @field groupvar A vector identifying groups in the panel
#' @field ngroup The number of groups in the panel
#' @field group_size A vector of size ngroups giving the number of observations in each group
#' @field unique_group_sizes Unique group sizes that appear in the dataset
#' @export ModelData
#' @exportClass ModelData
#' @import stringr methods utils
#' 
ModelData <- setRefClass(Class  = "ModelData",
                         fields = list(var                = "data.frame",
                                       varnames           = "character",
                                       nvars              = "numeric",
                                       nobs               = "numeric",
                                       const              = "list",
                                       groupvar           = "numeric",
                                       ngroup             = "numeric",
                                       group_size         = "numeric",
                                       unique_group_sizes = "numeric"
                         ),
                         methods = list(
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
                             }
                         )
)

ModelData$methods(
    setGroup = function(value) {
        "Add a vector identifying groups in the panel. This vector must be sorted in ascending
        order with length equal to the number of observations in the dataset.\n
        In most cases, the group variable is also in the data. It's better to first sort the data
        by the group variable, then create the ModelData object and set the group variable.\n
        \\code{value}: A vector identifying groups."
        
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
        "Add variables to columns.\n
        \\code{...}: The same as the constructor of data.frame.\n
        \\code{names}: Variable names"
        
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
        "Remove variables from columns.\n
        \\code{col}: Column index or variable names"

        if (is.character(col)) {
            if (any(!isVariable(col))) {
                stop(sprintf("%s not in the data", paste(c(col[!isVariable(col)]), 
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
        "Select a subset of data in the same way as subsetting a data.frame object.\n
        \\code{row}: Index or conditions for selecting rows. Default is all rows.\n
        \\code{col}: Index or variable names for selecting columns. Default is all columns."
        
        data <- data.frame(.self$var[row, col])
        if (is.character(col))
            colnames(data) <- col
        .self$var      <- data
        .self$varnames <- colnames(.self$var)
        .self$nvars    <- ncol(.self$var)
        .self$nobs     <- nrow(.self$var)
    },
    
    isVariable = function(varname) {
        "Determine if variables are in the dataset.\n
        \\code{varname}: The name of the variables."
        varname %in% .self$varnames
    },
    
    addArrayVars = function(data, name = "arrayvar") {
        "Add a matrix or 3-dimension variable in the field \\code{var}.\n
        \\code{data}: A matrix or a 3-dimension array.\n
        \\code{name}: The name of the array variable added."
        
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
        "Expand all array variables \\code{varname} with dimensions \\code{R} by \\code{C}
         into multiple variables with the variable name \\code{varname_array_[1:R]_[1:C]}.\n
         The reverse function of \\code{collapseArrayVars}."
        
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
        "Collapse all variables with the name \\code{varname_array_d1_d2} to an array variable 
         \\code{varname} with dimensions \\code{R} = \\code{max(d1)} by \\code{C = max(d2)}.\n
         The reverse function of \\code{expandArrayVars}."
        
        # Find names of all elements of array variables
        all_arrays <- grep("_array_[0-9]+_[0-9]+$", .self$varnames, value = TRUE)
        # Find names of all array variables
        arrayvars <- unique(sub("_array_[0-9]+_[0-9]+$", "", all_arrays))
        for (arrayvar in arrayvars) {
            # get all elements of the array variable
            arrays <- grep(arrayvar, all_arrays, value = TRUE)
            f <- function(array) stringr::str_extract(array, "[0-9]+_[0-9]+$")
            # get all suffix of the array variable
            collection <- sapply(arrays, f)
            # get the dimension of the array variable
            maxDimension <- as.integer(strsplit(max(collection), "_")[[1]])
            # initialize an array with maxDimension
            newArray <- array(0, c(.self$nobs, maxDimension))
            for (array in arrays) {
                # extract the location of each element of the array variable
                loc <- as.integer(strsplit(stringr::str_extract(array, "[0-9]+_[0-9]+$"), "_")[[1]])
                newArray[, loc[1], loc[2]] = .self$var[[array]]
                .self$removeData(array)
            }
            .self$addArrayVars(newArray, name = arrayvar)
        }
    }
)
