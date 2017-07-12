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
        unique_obs <- unique(nobs_by_group)
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
        result <- allocateToGroups(nobs_by_group, gnodes, inodes, weights, numnodes, nodeindex)
        gnodes    <- result$gnodes
        inodes    <- result$inodes
        weights   <- result$weights
        numnodes  <- result$numnodes
        nodeindex <- result$nodeindex
        
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
        gnodes  <- reshapeNodemat(matrix(rep(nodes[, 1 : ngroup], nobs), nrow = numnodes), numnodes, nobs, ngroup)
    } else {
        gnodes <- matrix(0, numnodes * nobs, 0)
    }
    if (nindiv) {
        inodes  <- reshapeNodemat(nodes[, (ngroup + 1) : ncol(nodes)], numnodes, nobs, nindiv)
    } else {
        gnodes <- matrix(0, numnodes * nobs, 0)
    }
    nodeindex <- reshapeNodemat(matrix(rep(1 : numnodes, nobs), nrow = numnodes), numnodes, nobs, 1)
    return(list(gnodes    = gnodes,
                inodes    = inodes,
                weights   = weights,
                numnodes  = numnodes,
                nodeindex = nodeindex))
}

reshapeNodemat <- function(nodemat, nnodes, nobs, nvars) {
    "Take input nodemat with nnodes rows and nobs*nvars columns and output reshaped array with 
    rows*nobs rows and nvars columns, where the rows of the output are arranged in nnodes blocks of nobs"
    temp <- array(nodemat, c(nnodes, nvars, nobs))
    temp <- aperm(temp, c(1, 3, 2))
    nodemat_out <- array(temp, c(nnodes * nobs, nvars))
    return (nodemat_out)
}

allocateToGroups <- function(nobs_by_group, gnodes, inodes, weights, numnodes, nodeindex) {
    gnodes    <- do.call(rbind, gnodes[nobs_by_group])
    inodes    <- do.call(rbind, inodes[nobs_by_group])
    weights   <- do.call(rbind, weights[nobs_by_group])
    numnodes  <- do.call(rbind, numnodes[nobs_by_group])
    nodeindex <- do.call(rbind, nodeindex[nobs_by_group])
    return(list(gnodes    = gnodes,
                inodes    = inodes,
                weights   = weights,
                numnodes  = numnodes,
                nodeindex = nodeindex))
}

arrange_weights <- function(data, raw_weights, numnodes) {
    weights <- list()
    weights$wgt   <- raw_weights
    weights$group <- expand_array((1:data$ngroups), numnodes)
    weights$node  <- seqwithin(weights$group)
    return (weights)
}

arrange_nodes <- function(model, data, gnodes, inodes, numnodes, nodeindex) {
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
            nodes$values[[name]] = inodes[, i]
        }
    }
    nodes$nodenum <- nodeindex
    nodes$obs <- expand_array(data$var$obsindex, numnodes[match(data$groupvar, groups)])
    nodes$group <- expand_array(data$groupvar, numnodes[match(data$groupvar, groups)])
    return (nodes)
}
