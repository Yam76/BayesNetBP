Triangulate <- function(graph, elim.order){
  Triangulate_orig(graph, elim.order)

}


Triangulate_orig <- function(graph, elim.order){
  dag.graph <- igraph.from.graphNEL(graph, weight=FALSE)
  # iterate over all nodes by elimination order
  for (i in 1:length(elim.order)){
    node <- elim.order[i]
    neighbors <- names(neighbors(dag.graph, node, mode="all"))
    n.neighbors <- length(neighbors)


    if (n.neighbors>=2){
      # iterate over all pairs of neighbors
      for (k1 in 1:(n.neighbors-1)){
        for (k2 in (k1+1):n.neighbors){
          # if both of the neighbor appears later in EO & are not connected
          # then connect them
          if (which(elim.order==neighbors[k1])>i){
            if (which(elim.order==neighbors[k2])>i){
              if(!are_adjacent(dag.graph, neighbors[k1], neighbors[k2])){
                dag.graph <- add_edges(dag.graph, c(neighbors[k1],neighbors[k2]))
              }
            }
          }
        }
      }
    }


  }
  dag.tri <- igraph.to.graphNEL(dag.graph)
  return(dag.tri)
}

Triangulate_test <- function(graph, elim.order){
  dag.graph <- graph

  # dag.graph <- graph
  # dag.graph@edgeL <- dag.graph@edgeL[elim.order]
  # dag.graph <- igraph.from.graphNEL(dag.graph, weight=FALSE)
  # # create adjacency matrix with elim.order as the order of columns and rows
  # #   adjacent to self is FALSE
  # dag_matrix <- as_adj(dag.graph, type = "both", sparse = FALSE) # TODO: does sparse need to be FALSE?
  # dag_matrix <- dag_matrix == 1 # convert to bool

  # create adjacency matrix with elim.order as the order of columns and rows
  #   adjacent to self is FALSE
  dag_matrix <- as(dag.graph, "matrix")[elim.order, elim.order]
  dag_matrix <- dag_matrix == 1


  for(i in 1:nrow(dag_matrix)){
    # get neighbors that are after ith node in elim.order
    # get neighbors
    neighbors <- elim.order[dag_matrix[i, ]]
    # keep only neighbors after ith node in elim.order
    neighbors <- neighbors[match(neighbors, elim.order) > i]

    if(length(neighbors) >= 2){
      for(n1 in 1:(length(neighbors)-1)){
        for(n2 in (n1+1):length(neighbors)){
          if(!dag_matrix[n1, n2]){
            dag.graph <- add_edges(dag.graph, c(neighbors[n1], neighbors[n2]))
            dag_matrix[n1, n2] <- TRUE
          }
        }
      }
    }

  }

  dag.tri <- igraph.to.graphNEL(dag.graph)
  return(dag.tri)
}



