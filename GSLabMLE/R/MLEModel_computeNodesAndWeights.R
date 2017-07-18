#' @include MLEModel.R
MLEModel$methods(
    computeNodesAndWeights = function(data, quadacc) {
        "\\subsection{Description}{
        Compute nodes and weights for numerical integration.\n
        The complexity in the code here comes from the fact that different groups may have different
        numbers of unobservables (if there are individual-level unobservables and the panel is
        unbalanced), and we need to arrange the nodes for each unobservable into a single vector to
        speed numerical integration.}
        \\subsection{Parameters}{
        \\code{data}: An \\code{MLEData} object.\n
        \\code{quadacc}: The accuracy.\n}
        \\subsection{Return}{
        \\code{nodes}:\n
        \\code{nodes$value}: A list of nodes with one field for each unobservable.\n
        \\code{nodes$group}: A single-column matrix indexing the group variable.\n
        \\code{nodes$nodenum}: A single-column matrix indexing the order of node within each group.\n
        \\code{nodes$obs}: A single-column matrix indexing each observation.\n
        \\code{weights}:\n
        \\code{weights$wgt}: A single-column matrix representing weight for each group.\n
        \\code{weights$group}: A single-column matrix indexing the group variable.\n
        \\code{weights$node}: A single-column matrix indexing the order of node within each group.
        \\code{data_rep}: An \\code{MLEData} object with each observations replicated by the number
        of nodes depending on the number of unobservables and accuracy. The nodes for unobservables
        are added in the data. The number of groups is also multiplied by the number of nodes.}"
        if (.self$numerical_integral) {
            if (!length(data$groupvar)) {
                data$setGroup(data$var$obsindex)   
            }
            result <- sumWithin(rep(1, data$nobs), data$groupvar)
            nobs_by_group <- result$value
            group         <- result$group
            group_mapping <- match(data$groupvar, group)
            unique_obs    <- unique(nobs_by_group)
            gnodes    <- list()
            inodes    <- list()
            weights   <- list()
            numnodes  <- list()
            nodeindex <- list()
            for (nobs in unique_obs) {
                result <- GSLabMLE:::getRaw(nobs, .self$nindiv_unobs, .self$ngroup_unobs, quadacc)
                gnodes[[nobs]]    <- result$gnodes
                inodes[[nobs]]    <- result$inodes
                weights[[nobs]]   <- result$weights
                numnodes[[nobs]]  <- result$numnodes
                nodeindex[[nobs]] <- result$nodeindex
            }
            gnodes    <- GSLabMLE:::allocateToGroups(nobs_by_group, gnodes)
            inodes    <- GSLabMLE:::allocateToGroups(nobs_by_group, inodes)
            weights   <- GSLabMLE:::allocateToGroups(nobs_by_group, weights)
            numnodes  <- GSLabMLE:::allocateToGroups(nobs_by_group, numnodes)
            nodeindex <- GSLabMLE:::allocateToGroups(nobs_by_group, nodeindex)
            weights   <- GSLabMLE:::arrangeWeights(data, weights, numnodes, group)
            nodes     <- GSLabMLE:::arrangeNodes(.self, data, gnodes, inodes, numnodes, nodeindex, group_mapping)
            data_rep  <- data$copy()
            data_rep$selectData(nodes$obs)
            data_rep$setGroup(groups(cbind(nodes$group, nodes$nodenum)))
        } else {
            nodes    <- list()
            weights  <- list()
            data_rep <- data$copy()
        }
        return (list(nodes    = nodes,
                     weights  = weights,
                     data_rep = data_rep))
    }
)

getRaw <- function(nobs, nindiv, ngroup, quadacc) {
    dim      <- nobs * nindiv + ngroup
    result   <- createSparseGrid("KPN", dim, quadacc)
    nodes    <- as.matrix(result$nodes)
    weights  <- as.matrix(result$weights)
    numnodes <- dim(nodes)[1]
    if (ngroup) {
        gnodes <- GSLabMLE:::reshapeNodemat(matrix(rep(nodes[, 1 : ngroup], nobs), nrow = numnodes), nobs, ngroup)
    } else {
        gnodes <- matrix(0, numnodes * nobs, 0)
    }
    if (nindiv) {
        inodes <- GSLabMLE:::reshapeNodemat(nodes[, (ngroup + 1) : ncol(nodes)], nobs, nindiv)
    } else {
        inodes <- matrix(0, numnodes * nobs, 0)
    }
    nodeindex <- GSLabMLE:::reshapeNodemat(matrix(rep(1 : numnodes, nobs), nrow = numnodes), nobs, 1)
    return(list(gnodes    = gnodes,
                inodes    = inodes,
                weights   = weights,
                numnodes  = numnodes,
                nodeindex = nodeindex))
}

reshapeNodemat <- function(nodes, nobs, nvars) {
    "Reshape a node matrix. 
     The function reshapes the nodes with \\code{nobs}*\\code{nvars} columns into a matrix
     with \\code{nvars} columns The rows of the output are arranged in \\code{nobs} block.
     \\code{nodemat A matrix with \\code{nobs}*\\code{nvars} columns.
     \\code{nobs} The number of observations.
     \\code{nvars} The number of variables. Also equal to the number of columns in the result matrix.
     nodemat <- matrix(c(1:6,6:1), 2, byrow = TRUE)
     reshapeNodemat(nodemat, 3, 2)"
    numnodes <- nrow(nodes)
    temp     <- array(nodes, c(numnodes, nvars, nobs))
    temp     <- aperm(temp, c(1, 3, 2))
    nodes    <- matrix(temp, nrow = numnodes * nobs)
    return (nodes)
}

allocateToGroups <- function(nobs_by_group, var) {
    return (do.call(rbind, var[nobs_by_group]))
}

arrangeWeights <- function(data, raw_weights, numnodes, group) {
    weights       <- list()
    weights$wgt   <- raw_weights
    weights$group <- as.matrix(group[expandArray(1:data$ngroup, numnodes)])
    weights$node  <- seqWithin(weights$group)$indices
    return (weights)
}

arrangeNodes <- function(model, data, gnodes, inodes, numnodes, nodeindex, group_mapping) {
    nodes <- list()
    if (model$ngroup_unobs) {
        for (i in 1 : model$ngroup_unobs) {
            name <- model$group_unobs_list[i]
            nodes$values[[name]] <- gnodes[,i]
        }
    }
    if (model$nindiv_unobs) {
        for (i in 1 : model$nindiv_unobs) {
            name <- model$indiv_unobs_list[i]
            nodes$values[[name]] <- inodes[, i]
        }
    }
    nodenum <- nodeindex
    obs     <- expandArray(data$var$obsindex, numnodes[group_mapping])
    group   <- expandArray(data$groupvar, numnodes[group_mapping])
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
