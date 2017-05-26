modelData <- setRefClass("modelData",
                         fields = list(var      = "data.frame",
                                       varnames = "character",
                                       nobs     = "numeric",
                                       nvars    = "numeric"),
                         methods = list(
                           initialize = function(data = NULL, varnames = NULL) {
                             if (is.null(data)) return (.self)
                             newdata <- data.frame(data)
                             if (!is.null(varnames)) {
                               colnames(newdata) <- varnames
                             } else if (is.null(colnames(data))) {
                               colnames(newdata) <- paste("var", seq(from = 1, to = ncol(newdata)), sep = "")
                             }
                             .self$var      <- newdata
                             .self$varnames <- colnames(newdata)
                             .self$nobs     <- nrow(newdata)
                             .self$nvars    <- ncol(newdata)
                             validObject(.self)
                           },
                           
                           addData = function(data, names = NULL) {
                             newdata <- data.frame(data)
                             if (nrow(newdata) != .self$nobs) {
                               stop("Number of observations is mismatched.")
                             } else if (!is.null(names)) {
                               colnames(newdata) = names
                             } else if (is.null(colnames(data))) {
                               names <- paste("var", seq(from = 1,to = .self$nvars + ncol(newdata)), sep = "")
                               colnames(newdata) <- names[!names %in% .self$varnames][1:ncol(newdata)]
                             }
                             .self$var      <- data.frame(.self$var, newdata)
                             .self$nvars    <- ncol(.self$var)
                             .self$varnames <- colnames(.self$var)
                           },
                           
                           removeData = function(col) {
                             if (is.numeric(col)) {
                               col <- .self$varnames[col]
                             }
                             .self$varnames <- .self$varnames[!.self$varnames %in% col]
                             .self$var      <- subset(.self$var, select = .self$varnames)
                             .self$nvars    <- ncol(.self$var)
                           },
                           
                           selectData = function(row = 1:nobs, col = .self$varnames) {
                             data = data.frame(.self$var[row, col])
                             if (is.character(col)) {
                               colnames(data) = col
                             }
                             .self$var      <- data
                             .self$varnames <- colnames(data)
                             .self$nobs     <- nrow(data)
                             .self$nvars    <- ncol(data)
                           }
                         )
)

setValidity("modelData", function(object) {
  if (any(duplicated(object$varnames))) {
    stop(sprintf("Duplicated variable names: %s", 
                 paste(object$varnames[duplicated(object$varnames)], collapse = ", ")))
  } else if (any(is.na(object$varnames))) {
    stop("Missing variable names")
  } else if (any(colnames(object$var) != object$varnames)) {
    stop("Wrong column names.")
  } else TRUE
}
)