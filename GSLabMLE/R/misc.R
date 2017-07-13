sumWithin <- function(value, group) {
    group <- as.matrix(group)
    value <- as.matrix(value)
    ncol1 <- ncol(group)
    ncol2 <- ncol(value)
    data <- data.frame(group, value)
    data <- aggregate(data[(ncol1 + 1) : (ncol1 + ncol2)], by = data[1 : ncol1], FUN = sum)
    group <- unname(as.matrix(data[1 : ncol1]))
    value <- unname(as.matrix(data[(ncol1 + 1) : (ncol1 + ncol2)]))
    return (list(group = group, value = value))
}

prodWithin <- function(value, group) {
    group <- as.matrix(group)
    value <- as.matrix(value)
    ncol1 <- ncol(group)
    ncol2 <- ncol(value)
    data <- data.frame(group, value)
    data <- aggregate(data[(ncol1 + 1) : (ncol1 + ncol2)], by = data[1 : ncol1], FUN = prod)
    group <- unname(as.matrix(data[1 : ncol1]))
    value <- unname(as.matrix(data[(ncol1 + 1) : (ncol1 + ncol2)]))
    return (list(group = group, value = value))
}

avgWithin <- function(value, group) {
    group <- as.matrix(group)
    value <- as.matrix(value)
    ncol1 <- ncol(group)
    ncol2 <- ncol(value)
    data <- data.frame(group, value)
    data <- aggregate(data[(ncol1 + 1) : (ncol1 + ncol2)], by = data[1 : ncol1], FUN = mean)
    group <- unname(as.matrix(data[1 : ncol1]))
    value <- unname(as.matrix(data[(ncol1 + 1) : (ncol1 + ncol2)]))
    return (list(group = group, value = value))
}

#' Replicate each row of a matrix a specified number of times.
#' @param array A matrix whose rows will be replicated.
#' @param counts A vector or a single-column matrix with length equal to the number of rows in
#' \code{array}. The \code{i}th row of \code{array} will be replicated \code{counts[i]} times.
expandArray <- function(array, counts) {
    if (!is.matrix(array)) {
        stop("The first argument must be a matrix")
    }
    if (dim(array)[1] != length(counts)) {
        stop("Array and counts must have the same rows")
    }
    expanded_indices <- rep(1:length(counts), counts)
    expanded_array   <- as.matrix(array[expanded_indices, ])
    return (expanded_array)
}
