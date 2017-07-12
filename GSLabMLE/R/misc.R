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