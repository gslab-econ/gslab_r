ModelData <- setClass("ModelData",
                      slots     = c(var      = "data.frame",
                                    varnames = "character",
                                    nobs     = "numeric",
                                    nvars    = "numeric"),
                      prototype = list(var      = data.frame(),
                                       varnames = as.character(NA),
                                       nobs     = as.numeric(NA),
                                       nvars    = as.numeric(NA)),
                      validity  = function(object) {
                        if (any(duplicated(object@varnames))) {
                          return(sprintf("Duplicated variable names: %s", 
                                          paste(object@varnames[duplicated(object@varnames)],
                                                collapse = ", ")))
                        }
                        if (object@nvars != ncol(object@var) | 
                            object@nvars != length(object@varnames)){
                          return("Wrong number of variables.")
                        }
                        if (nrow(object@var) != object@nobs) {
                          return("Wrong number of observations.")
                        }
                        if (any(colnames(object@var) != object@varnames)) {
                          return("Wrong column names.")
                        }
                        return(TRUE)
                      }
)

# create ModelData method (initialize)
setGeneric(name = "ModelData",
           def  = function(data, varnames = NULL) {
             standardGeneric("ModelData")
           }
)
setMethod(f = "ModelData", signature = c("data.frame"),
          definition = function(data, varnames = NULL) {
            if(!is.null(varnames)) {
              if(length(varnames) != ncol(data)) {
                stop("Wrong number of variable names supplied")
              } else {
                colnames(data) <- varnames
              }
            }
            obj = new("ModelData",
                      var      = data,
                      varnames = colnames(data),
                      nobs     = nrow(data),
                      nvars    = ncol(data))
            validObject(obj)
            return(obj)
          }
)
setMethod(f = "ModelData", signature = c("matrix"),
          definition = function(data, varnames = NULL) {
            data <- data.frame(data)
            return(ModelData(data, varnames))
          }
)
setMethod(f = "ModelData", signature = c("numeric"),
          definition = function(data, varnames = NULL) {
            if (is.null(varnames)) {
              varnames = "var1"
            }
            data <- data.frame(data)
            return(ModelData(data, varnames))
          }
)

# create removeData method
setGeneric(name = "removeData",
           def  = function(obj, varnames) {
             standardGeneric("removeData")
           }
)
setMethod(f = "removeData", signature = c("ModelData", "character"),
          definition = function(obj, varnames) {
            if (all(varnames %in% obj@varnames)) {
              obj@varnames = obj@varnames[!obj@varnames %in% varnames]
              obj@var <- subset(obj@var, select = obj@varnames)
              obj@nvars = ncol(obj@var)
            } else {
              stop(sprintf("%s not in the data", 
                           paste(c(varnames[!varnames %in% obj@varnames]), collapse = ", ")))
            }
            validObject(obj)
            return(obj)
          }
)

# create addData method
setGeneric(name = "addData",
           def  = function(obj, data, varnames = NULL) {
             standardGeneric("addData")
           }
)
setMethod(f = "addData", signature = c("ModelData", "data.frame"),
          definition = function(obj, data, varnames = NULL) {
            if (nrow(data) !=  obj@nobs) {
              stop("Number of observations is mismatched.")
            }
            if (!is.null(varnames)) {
              if (length(varnames) != ncol(data)) {
                stop("Wrong number of variable names supplied")
              } else {
                colnames(data) = varnames
              }
            }
            obj@var      <- cbind(obj@var, data)
            obj@nvars    <- ncol(obj@var)
            obj@varnames <- colnames(obj@var)
            validObject(obj)
            return(obj)
          }
)
setMethod(f = "addData", signature = c("ModelData", "matrix"),
          definition = function(obj, data, varnames = NULL) {
            data <- data.frame(data)
            return(addData(obj, data, varnames))
          }
)
setMethod(f = "addData", signature = c("ModelData", "numeric"),
          definition = function(obj, data, varnames = NULL) {
            data <- data.frame(data)
            if (is.null(varnames)) {
              suffix = min(which(c(paste("var", seq(from = 1,to = obj@nvars + 1),
                                         sep = "") %in% obj@varnames) == FALSE))
              colnames(data) <- paste("var", suffix, sep = "")
            }
            return(addData(obj, data, varnames))
          }
)

# create selectData method
setGeneric(name = "selectData",
           def  = function(obj, row = NULL, col = NULL) {
             standardGeneric("selectData")
           }
)
setMethod(f = "selectData", signature = c("ModelData"),
          definition = function(obj, row = NULL, col = NULL) {
            if (is.null(row) & is.null(col)) {
              return(obj)
            } else {
              if (!is.null(row)) {
                obj@var  <- as.data.frame(obj@var[row, ])
                obj@nobs <- nrow(obj@var)
                colnames(obj@var) = obj@varnames
              }
              if (!is.null(col)) {
                obj@var      <- as.data.frame(obj@var[, col])
                if (is.character(col)) {
                  colnames(obj@var) = col
                }
                obj@varnames <- colnames(obj@var)
                obj@nvars    <- ncol(obj@var)
              }
            }
            validObject(obj)
            return(obj)
          }
)
