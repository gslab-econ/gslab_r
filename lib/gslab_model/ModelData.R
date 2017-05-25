ModelData <- setClass("ModelData",
                      slots    = c(var      = "data.frame",
                                   varnames = "character",
                                   nobs     = "numeric",
                                   nvars    = "numeric"),
                      validity = function(object) {
                        if (any(duplicated(object@varnames))) {
                          return (sprintf("Duplicated variable names: %s", 
                                         paste(object@varnames[duplicated(object@varnames)], collapse = ", ")))
                        } else if (object@nvars != ncol(object@var) | object@nvars != length(object@varnames)) {
                          return ("Wrong number of variables.")
                        } else if (nrow(object@var) != object@nobs) {
                          return ("Wrong number of observations.")
                        } else if (any(colnames(object@var) != object@varnames)) {
                          return ("Wrong column names.")
                        } else {
                          return (TRUE)
                        }
                      }
)

# create ModelData method (initialize)
setGeneric(name = "ModelData",
           def  = function(data, varnames = NULL) {
             standardGeneric("ModelData")
           }
)
setMethod(f = "ModelData",
          definition = function(data, varnames = NULL) {
            newdata <- data.frame(data)
            if(!is.null(varnames)) {
              if(length(varnames) != ncol(newdata)) {
                stop("Wrong number of variable names supplied")
              } else {
                colnames(newdata) <- varnames
              }
            } else if (is.null(colnames(data))) {
              colnames(newdata) <- paste("var", seq(from = 1,to = ncol(newdata)), sep = "")
            }
            obj = new("ModelData",
                      var      = newdata,
                      varnames = colnames(newdata),
                      nobs     = nrow(newdata),
                      nvars    = ncol(newdata))
            validObject(obj)
            return (obj)
          }
)

# create addData method
setGeneric(name = "addData",
           def  = function(obj, data, varnames = NULL) {
             standardGeneric("addData")
           }
)
setMethod(f = "addData", signature = c("ModelData"),
          definition = function(obj, data, varnames = NULL) {
            newdata <- data.frame(data)
            if (nrow(newdata) != obj@nobs) {
              stop("Number of observations is mismatched.")
            }
            if (!is.null(varnames)) {
              if (length(varnames) != ncol(newdata)) {
                stop("Wrong number of variable names supplied")
              } else {
                colnames(newdata) = varnames
              }
            } else if (is.null(colnames(data))) {
              varnames <- paste("var", seq(from = 1,to = obj@nvars + ncol(newdata)), sep = "")
              colnames(newdata) <- varnames[!varnames %in% obj@varnames][1:ncol(newdata)]
            }
            obj@var      <- cbind(obj@var, newdata)
            obj@nvars    <- ncol(obj@var)
            obj@varnames <- colnames(obj@var)
            validObject(obj)
            return (obj)
          }
)

# create removeData method
setGeneric(name = "removeData",
           def  = function(obj, col) {
             standardGeneric("removeData")
           }
)
setMethod(f = "removeData", signature = c("ModelData"),
          definition = function(obj, col) {
            if (is.numeric(col)) {
              col <- obj@varnames[col]
            }
            if (all(col %in% obj@varnames)) {
              obj@varnames <- obj@varnames[!obj@varnames %in% col]
              obj@var      <- subset(obj@var, select = obj@varnames)
              obj@nvars    <- ncol(obj@var)
            } else {
              stop(sprintf("%s not in the data", paste(c(col[!col %in% obj@varnames]), collapse = ", ")))
            }
            validObject(obj)
            return (obj)
          }
)

# create selectData method
setGeneric(name = "selectData",
           def  = function(obj, row = 1:obj@nobs, col = obj@varnames) {
             standardGeneric("selectData")
           }
)
setMethod(f = "selectData", signature = c("ModelData"),
          definition = function(obj, row = 1:obj@nobs, col = obj@varnames) {
            data = data.frame(obj@var[row, col])
            if (is.character(col)) {
              colnames(data) = col
            }
            obj@var      = data
            obj@varnames = colnames(data)
            obj@nobs     = nrow(data)
            obj@nvars    = ncol(data)
            validObject(obj)
            return (obj)
          }
)