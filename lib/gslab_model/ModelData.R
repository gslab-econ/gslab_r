modelData <- setRefClass("modelData",
                         fields = list(var      = "data.frame",
                                       varnames = "character",
                                       nobs     = "numeric",
                                       nvars    = "numeric"),
                         methods = list(
                           initialize = function(data = NULL, varnames = NULL) {
                             if (is.null(data)) {
                               return (.self)
                             }
                             newdata <- data.frame(data)
                             if(!is.null(varnames)) {
                               if(length(varnames) != ncol(newdata)) {
                                 stop("Wrong number of variable names supplied")
                               } else {
                                 colnames(newdata) <- varnames
                               }
                             } else if (is.null(colnames(data))) {
                               colnames(newdata) <- paste("var", seq(from = 1, to = ncol(newdata)), sep = "")
                             }
                             if (any(duplicated(varnames))) {
                               stop(sprintf("Duplicated variable names: %s", 
                                            paste(varnames[duplicated(varnames)], collapse = ", ")))
                             }
                             .self$var      <- newdata
                             .self$varnames <- colnames(newdata)
                             .self$nobs     <- nrow(newdata)
                             .self$nvars    <- ncol(newdata)
                           },
                           
                           addData = function(data, names = NULL) {
                             newdata <- data.frame(data)
                             if (nrow(newdata) != .self$nobs) {
                               stop("Number of observations is mismatched.")
                             }
                             if (!is.null(names)) {
                               if (length(names) != ncol(newdata)) {
                                 stop("Wrong number of variable names supplied")
                               } else {
                                 colnames(newdata) = names
                               }
                             } else if (is.null(colnames(data))) {
                               names <- paste("var", seq(from = 1,to = .self$nvars + ncol(newdata)), sep = "")
                               colnames(newdata) <- names[!names %in% .self$varnames][1:ncol(newdata)]
                             }
                             if (any(colnames(newdata) %in% .self$varnames)) {
                               stop(sprintf("Duplicated variable names: %s",
                                            paste(colnames(newdata)[colnames(newdata) %in% names]), sep = ", "))
                             }
                             .self$var      <- cbind(.self$var, newdata)
                             .self$nvars    <- ncol(.self$var)
                             .self$varnames <- colnames(.self$var)
                           },
                           
                           removeData = function(col) {
                             if (is.numeric(col)) {
                               col <- .self$varnames[col]
                             }
                             if (all(col %in% .self$varnames)) {
                               .self$varnames <- .self$varnames[!.self$varnames %in% col]
                               .self$var      <- subset(.self$var, select = .self$varnames)
                               .self$nvars    <- ncol(.self$var)
                             } else {
                               stop(sprintf("%s not in the data", paste(c(col[!col %in% .self$varnames]),
                                                                        collapse = ", ")))
                             }
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