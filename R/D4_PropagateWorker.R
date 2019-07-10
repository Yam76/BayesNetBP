###########################################
## Propagate
###########################################

propagate.worker <- function(tree.graph, potentials, cluster.sets){

  # tree.graph <- tree.sub.graph; potentials <- potentials.sub; cluster.sets <- discrete.sets

  cluster.tree <- list(
    # bn=dag,
    tree=tree.graph,
    clusters=cluster.sets,
    # assignment=asgn,
    collected=c(), active=c(), potentials=potentials, joint=potentials)

  clusters <- names(potentials)
  result <- list()

  ## NEW version of getting joints

  # collect
  ce <- CollectEvidence(cluster.tree, clusters[1])
  # reset active nodes
  ce$active <- c()
  # distribute
  de <- DistributeEvidence(ce, clusters[1])
  result <- de$joint

  return(result)
}

# ppgt <- Propagate(tree.sub.graph, potentials.sub, discrete.sets)

###################################################

Absorb <- function(absorbedTo, absorbedFrom, separator, distribute=FALSE){
  pot1 <- absorbedFrom
  pot2 <- absorbedTo

  # pot2 <- cluster.tree$potentials[["Cyp2b10"]]; pot1 <- cluster.tree$potentials[["HDL"]];
  # separator <- intersect( cluster.tree$clusters[["Cyp2b10"]],  cluster.tree$clusters[["HDL"]])

  inter.var <- separator
  sep <- marginalize.discrete(pot1, inter.var)

  results <- list()
  if (distribute) {
    results[[1]] <- NULL
  } else {
    results[[1]] <- factor.divide(pot1, sep)
  }

  results[[2]] <- factor.product(pot2, sep)
  return(results)
}

###########################################
## Collect evidence
###########################################

CollectEvidence <- function(cluster.tree, node){
  CollectEvidence_test(cluster.tree, node)

}


# absorb nodes and then return which nodes were collected
CollectEvidence_test <- function(cluster.tree, node){
  clique.names <- names(V(cluster.tree$tree)) # get tree nodes
  nodes_status <- data.frame(clique.names, collected = FALSE, queued = FALSE) # initialize markings
  nodes_status[nodes_status$clique.names %in% node,]$queued <- TRUE # mark `node` as queued

  while(any(nodes_status$queued)){ # as long as there are nodes queued
    n <- nodes_status[nodes_status$queued,][1,1] # get the first node in queue
    nodes_status[nodes_status$queued,][1,]$collected <- TRUE # mark first node in queue as collected
    nodes_status[nodes_status$queued,][1,]$queued <- FALSE # unqueue the first node


    neighbors_names <- neighbors(cluster.tree$tree, n, mode = "all")$name # get neighbors of n
    neighbors_status <- nodes_status[nodes_status$clique.names %in% neighbors_names,] # get neighbors' rows
    uncollected <- neighbors_status[!neighbors_status$collected,] # cull for only uncollected neighbors
    uncollected_names <- uncollected$clique.names # get the names of uncollected neighbors

    if(length(uncollected_names) > 0){ # absorb magic
      nodes_status[nodes_status$clique.names %in% uncollected$clique.names, ]$queued <- TRUE # queue uncollected neighbors

      for (i in 1:length(uncollected_names)) {
        abb <- Absorb(cluster.tree$potentials[[n]], cluster.tree$potentials[[uncollected_names[i]]],
                      separator = intersect( cluster.tree$clusters[[n]],  cluster.tree$clusters[[uncollected_names[i]]]))
        cluster.tree$potentials[[n]] <- abb[[2]]
        cluster.tree$potentials[[uncollected_names[i]]] <- abb[[1]]
      }
    }
  }

  cluster.tree$collected <- nodes_status[nodes_status$collected]$clique.names
  cluster.tree$active <- nodes_status[nodes_status$collected]$clique.names
  return(cluster.tree)

  # for(j in 1:length(clique.names)){
  #   nodes_status[j, 2] <- TRUE
  #   neighbors <- neighbors(cluster.tree$tree, n, mode = "all")$name
  #   neighbors_status <- nodes_status[nodes_status$clique.names %in% neighbors,]
  #
  #   inactive <- neighbors_status[!neighbors_status$active] # only get inactive neighbors
  #   if(length(inactive) > 0){
  #
  #
  #     for (i in 1:nrow(inactive)) {
  #       abb <- Absorb(cluster.tree$potentials[[node]], cluster.tree$potentials[[inactive[i]]],
  #                     separator = intersect( cluster.tree$clusters[[node]],  cluster.tree$clusters[[inactive[i]]]))
  #       cluster.tree$potentials[[node]] <- abb[[2]]
  #       cluster.tree$potentials[[inactive[i]]] <- abb[[1]]
  #       # cat(inactive[i], " -> ", node, "\n")
  #     }
  #
  #   }
  #   nodes_status[j, 3] <- TRUE
  # }
}

CollectEvidence_orig <- function(cluster.tree, node) {
  # print(c("Collecting node", node))

  clique.names <- names(V(cluster.tree$tree))
  ngbs <- neighbors(cluster.tree$tree, node, mode = "all")$name # neighbors of node
  inactive <- setdiff(ngbs, cluster.tree$active) # neighboring nodes that are not active
  cluster.tree$active <- c(cluster.tree$active, node) # node is now active

  for (ngb in inactive){ # for each inactive neighbor, we seek to activate them
    # cat("collecting for ", node, "from", ngb)
    cluster.tree <- CollectEvidence(cluster.tree, ngb)
    # collected <- ce[[1]]
    # cst <- ce[[2]]
  }

  if (length(inactive)>0) {
    for (i in 1:length(inactive)) {
      abb <- Absorb(cluster.tree$potentials[[node]], cluster.tree$potentials[[inactive[i]]],
                    separator = intersect( cluster.tree$clusters[[node]],  cluster.tree$clusters[[inactive[i]]]))
      cluster.tree$potentials[[node]] <- abb[[2]]
      cluster.tree$potentials[[inactive[i]]] <- abb[[1]]
      # cat(inactive[i], " -> ", node, "\n")
    }

  }
  cluster.tree$collected <- c(cluster.tree$collected, node)
  return(cluster.tree)
}

###########################################
## Distribute evidence
###########################################

DistributeEvidence <- function(cluster.tree, node){
  DistributeEvidence_test(cluster.tree, node)
}

DistributeEvidence_test <- function(cluster.tree, node){
  clique.names <- names(V(cluster.tree$tree))
  nodes_status <- data.frame(clique.names, active = FALSE, queued = FALSE, stringsAsFactors = FALSE) # are they actually inactive? NO, need to fix
  # but they actually are inactive, since propagate.worker clears cluster.tree$active!

  # nodes_status$active <- nodes_status$clique.names %in% cluster.tree$active
  nodes_status[nodes_status$clique.names %in% node,]$queued <- TRUE

  while(any(nodes_status$queued)){
    n <- nodes_status[nodes_status$queued,][1,1]
    nodes_status[nodes_status$queued,][1,]$active <- TRUE
    nodes_status[nodes_status$queued,][1,]$queued <- FALSE

    neighbors_names <- neighbors(cluster.tree$tree, n, mode = "all")$name # get neighbors of n
    neighbors_status <- nodes_status[nodes_status$clique.names %in% neighbors_names,] # get neighbors' rows
    inactive <- neighbors_status[!neighbors_status$active,] # cull for only uncollected neighbors
    inactive_names <- inactive$clique.names # get the names of uncollected neighbors

    if (length(inactive_names)>0) {
      nodes_status[nodes_status$clique.names %in% inactive$clique.names, ]$queued <- TRUE # queue inactive neighbors

      for (i in 1:length(inactive_names)) {
        abb <- Absorb(cluster.tree$potentials[[inactive_names[i]]], cluster.tree$potentials[[n]],
                      separator = intersect( cluster.tree$clusters[[n]],  cluster.tree$clusters[[inactive_names[i]]]),
                      distribute = TRUE)
        cluster.tree$potentials[[inactive_names[i]]] <- abb[[2]]
      }
    }

    cluster.tree$joint[[n]] <- cluster.tree$potentials[[n]]

  }
  cluster.tree$active <- nodes_status[nodes_status$active,]$clique.names

  return(cluster.tree)
}

# need to reset the active nodes of cluster.tree after collecting evidence

DistributeEvidence_orig <- function(cluster.tree, node){
  # print(c("Distributing node: ", node))

  clique.names <- names(V(cluster.tree$tree))
  ngbs <- neighbors(cluster.tree$tree, node, mode = "all")$name

  inactive <- setdiff(ngbs, cluster.tree$active)
  cluster.tree$active <- c(cluster.tree$active, node)

  cluster.tree$joint[[node]] <- cluster.tree$potentials[[node]]

  if (length(inactive)>0) {
    for (i in 1:length(inactive)) {
      abb <- Absorb(cluster.tree$potentials[[inactive[i]]], cluster.tree$potentials[[node]],
                    separator = intersect( cluster.tree$clusters[[node]],  cluster.tree$clusters[[inactive[i]]]),
                    distribute = TRUE)
      cluster.tree$potentials[[inactive[i]]] <- abb[[2]]
      cluster.tree <- DistributeEvidence(cluster.tree, inactive[i])
    }
  }

  return(cluster.tree)
}

