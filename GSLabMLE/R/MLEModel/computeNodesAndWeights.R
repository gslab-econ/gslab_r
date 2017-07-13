source("ExampleModel.R")

set.seed(1)
n     <- 10
mu    <- 1
sigma <- 2
param <- c(mu, sigma)
y     <- rnorm(n, mu, sigma)
group <- sort(sample(8, n, replace = TRUE))
data  <- MLEData(y)
data$setGroup(group)
model <- ExampleModel("y")
quadacc <- 3

computeNodesAndWeights <- function(.self, data, quadacc) {
    if (model$numerical_integral) {
        if (!length(data$groupvar)) {
            data$setGroup(data$var$obsindex)   
        }
        nobs_by_group <- sumWithin(rep(1, data$nobs), data$groupvar)$value
        groups        <- sumWithin(rep(1, data$nobs), data$groupvar)$group
        unique_obs    <- unique(nobs_by_group)
        gnodes    <- list()
        inodes    <- list()
        weights   <- list()
        numnodes  <- list()
        nodeindex <- list()
        for (nobs in unique_obs) {
            result <- getRaw(nobs, model$nindiv_unobs, model$ngroup_unobs, quadacc)
            gnodes[[nobs]]    <- result$gnodes
            inodes[[nobs]]    <- result$inodes
            weights[[nobs]]   <- result$weights
            numnodes[[nobs]]  <- result$numnodes
            nodeindex[[nobs]] <- result$nodeindex
        }
        gnodes    <- allocateToGroups(nobs_by_group, gnodes)
        inodes    <- allocateToGroups(nobs_by_group, inodes)
        weights   <- allocateToGroups(nobs_by_group, weights)
        numnodes  <- allocateToGroups(nobs_by_group, numnodes)
        nodeindex <- allocateToGroups(nobs_by_group, nodeindex)
        
        weights <- arrangeWeights(data, weights, numnodes)
        nodes   <- arrangeNodes(model, data, gnodes, inodes, numnodes, nodeindex)
        data_rep <- data$copy()
        data_rep$selectData(nodes$obs)
    } else {
        nodes         <- list()
        nodes$group   <- data$groupvar
        nodes$nodenum <- rep(1, data$nobs)
        nodes$obs     <- 1 : data$nobs
        nodes$values  <- list()
        weights       <- list()
        weights$group <- 1 : data$ngroup
        weights$wgt   <- c()
        data_rep      <- data$copy()
    }
    return (list(nodes    = nodes,
                 weights  = weights,
                 data_rep = data_rep))
}

getRaw <- function(nobs, nindiv, ngroup, quadacc) {
    dim <- nobs * nindiv + ngroup
    result   <- createSparseGrid("KPN", dim, quadacc)
    nodes    <- result$nodes
    weights  <- as.matrix(result$weights)
    numnodes <- dim(nodes)[1]
    if (ngroup) {
        gnodes <- reshapeNodemat(matrix(rep(nodes[, 1 : ngroup], nobs), nrow = numnodes), nobs, ngroup)
    } else {
        gnodes <- matrix(0, numnodes * nobs, 0)
    }
    if (nindiv) {
        inodes <- reshapeNodemat(nodes[, (ngroup + 1) : ncol(nodes)], nobs, nindiv)
    } else {
        inodes <- matrix(0, numnodes * nobs, 0)
    }
    nodeindex <- reshapeNodemat(matrix(rep(1 : numnodes, nobs), nrow = numnodes), nobs, 1)
    return(list(gnodes    = gnodes,
                inodes    = inodes,
                weights   = weights,
                numnodes  = numnodes,
                nodeindex = nodeindex))
}

#' Reshape a node matrix. 
#' @description The function reshapes the nodes with \code{nobs}*\code{nvars} columns into a matrix
#' with \code{nvars} columns The rows of the output are arranged in \code{nobs} block.
#' @param nodemat A matrix with \code{nobs}*\code{nvars} columns.
#' @param nobs The number of observations.
#' @param nvars The number of variables. Also equal to the number of columns in the result matrix.
#' @examples 
#' nodemat <- matrix(c(1:6,6:1), 2, byrow = TRUE)
#' reshapeNodemat(nodemat, 3, 2)
reshapeNodemat <- function(nodemat, nobs, nvars) {
    numnodes <- nrow(nodemat)
    temp <- array(nodemat, c(numnodes, nvars, nobs))
    temp <- aperm(temp, c(1, 3, 2))
    nodemat_out <- array(temp, c(numnodes * nobs, nvars))
    return (nodemat_out)
}

allocateToGroups <- function(nobs_by_group, var) {
    return (do.call(rbind, var[nobs_by_group]))
}

arrangeWeights <- function(data, raw_weights, numnodes) {
    weights <- list()
    weights$wgt   <- raw_weights
    weights$group <- expandArray(matrix(1:data$ngroup), numnodes[match(data$groupvar, groups)])
    #weights$node  <- seqwithin(weights$group)
    return (weights)
}

arrangeNodes <- function(model, data, gnodes, inodes, numnodes, nodeindex) {
    nodes <- list()
    if (model$ngroup_unobs) {
        for (i in 1 : model$ngroup_unobs) {
            name <- model$group_unobs_list[i]
            nodes$values[name] <- gnodes[,i]
        }
    }
    if (model$nindiv_unobs) {
        for (i in 1 : model$nindiv_unobs) {
            name <- model$indiv_unobs_list[i]
            nodes$values[[name]] <- inodes[, i]
        }
    }
    nodenum <- nodeindex
    obs     <- expand_array(matrix(data$var$obsindex), numnodes[match(data$groupvar, groups)])
    group   <- expand_array(matrix(data$groupvar), numnodes[match(data$groupvar, groups)])
    m <- cbind(group, nodenum, obs, 1:length(group))
    m <- m[order(m[, 1], m[, 2], m[, 3]), ]
    nodes$group   <- as.matrix(m[, 1])
    nodes$nodenum <- as.matrix(m[, 2])
    nodes$obs     <- as.matrix(m[, 3])
    index         <- as.matrix(m[, 4])
    for (name in names(nodes$values)) {
        nodes$values[[name]] <- as.matrix(nodes$values[[name]][index])
    }
    return (nodes)
}
