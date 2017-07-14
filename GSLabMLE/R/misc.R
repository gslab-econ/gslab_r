#' Compute group summation.
#' @description The function takes as input a value matrix and a group matrix. A group is defined by an entire
#' row in the group matrix. For each column of the value matrix, the function computes the summation  
#' of all values within each unique group.
#' @param value A matrix or a vector.
#' @param group A matrix or a vector. The number of rows is the same as \code{value}.
#' @return The function returns a list with two fields: group, which contains the unique groups,
#' and value, which contains the summation by group.
#' 
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

#' Compute group product.
#' @description The function takes as input a value matrix and a group matrix. A group is defined by an entire
#' row in the group matrix. For each column of the value matrix, the function computes the product
#' of all values within each unique group.
#' @param value A matrix or a vector.
#' @param group A matrix or a vector. The number of rows is the same as \code{value}.
#' @return The function returns a list with two fields: group, which contains the unique groups,
#' and value, which contains the products by group.
#' 
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

#' Compute group average.
#' @description The function takes as input a value matrix and a group matrix. A group is defined by an entire
#' row in the group matrix. For each column of the value matrix, the function computes the average
#' of all values within each unique group.
#' @param value A matrix or a vector.
#' @param group A matrix or a vector. The number of rows is the same as \code{value}.
#' @return The function returns a list with two fields: group, which contains the unique groups,
#' and value, which contains the averages value by group.
#' 
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
#' @param array A vector or a matrix whose rows will be replicated.
#' @param counts A vector or a single-column matrix with length equal to the number of rows in
#' \code{array}. The \code{i}th row of \code{array} will be replicated \code{counts[i]} times.
#' @return A matrix of which the number of rows is the summation of all elements in \code{counts}.
expandArray <- function(array, counts) {
    array <- as.matrix(array)
    if (dim(array)[1] != length(counts)) {
        stop("Array and counts must have the same rows")
    }
    expanded_indices <- rep(1:length(counts), counts)
    expanded_array   <- as.matrix(array[expanded_indices, ])
    return (expanded_array)
}

#' Create a vector indexing the groups defined by the columns of a matrix.
#' @param mat A vector or a matrix that defines groups.
#' @return A vector of group indices.
groups <- function(x) {
    mat   <- as.matrix(x)
    mat   <- cbind(mat, matrix(1 : nrow(mat)))
    mat   <- mat[do.call(order, lapply(1 : ncol(mat), function(i) mat[, i])), ]
    index <- as.numeric(mat[, ncol(mat)])
    mat   <- as.matrix(mat[, 1 : (ncol(mat) - 1)])
    diff  <- as.matrix(mat[1 : (nrow(mat) - 1), ] == mat[2 : (nrow(mat)), ])
    diff  <- apply(diff, 1, all)
    v     <- c(1, cumsum(1 - diff) + 1)
    v[index] <- v
    return (v)
}

#' Sort group variable and create index within each group.
#' @description The function takes as input a group matrix. A group is defined by an entire
#' row in the group matrix. The function sort the group matrix by first column, then second, ...
#' It also return a single-column matrix that indicates the index of each row within each group.
#' @param group A matrix or a vector to be sorted.
#' @return The function returns a list with two fields: \code{sorted_group}, which contains the
#' sorted group matrix, and \code{indices}, which contains the index.
#' @examples 
#' seqWithin(matrix(c(2, 3, 1, 1, 3, 3, 2, 2, 3, 3, 1, 1, 3, 3), 7, byrow = TRUE))
#' 
seqWithin <- function(group) {
    group        <- as.matrix(group)
    sorted_group <- as.matrix(group[do.call(order, lapply(1 : ncol(group), function(i) group[, i])), ])
    indices      <- matrix(1, nrow(sorted_group))
    for (i in 2:nrow(sorted_group)) {
        if (all(sorted_group[i, ] == sorted_group[i - 1, ])) {
            indices[i] = indices[i - 1] + 1
        } else {
            indices[i] = 1
        }
    }
    return (list(indices      = indices,
                 sorted_group =  sorted_group))
}