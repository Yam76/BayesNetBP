

#' @importFrom igraph get.edge.ids
#'
#' @examples
#' data(liver)
#' elim.order <- EliminationOrder(liver$dag, liver$node.class)
#' graph.mor <- Moralize(liver$dag)
#' graph.tri_correct <- Triangulate_orig(graph.mor, elim.order)
#' graph.tri <- Triangulate_test(graph.mor, elim.order)
#' identical(graph.tri, graph.tri_correct)
#'

Triangulate <- function(graph, elim.order){
  Triangulate_test2(graph, elim.order)

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

# given a magnitude, generate all possible pairs of the numbers from 1 to magnitude inclusive. Order is not considered.

generate_pairs <- function(magnitude){
  if(magnitude <= 1) return(list())

  enumeration <- matrix(nrow = (magnitude-1)*magnitude/2, ncol = 2)

  sum = 0
  for(i in 1:(magnitude-1)){
    enumeration[i:(magnitude-1) + sum, 1] <- i
    enumeration[i:(magnitude-1) + sum, 2] <- (i+1):magnitude

    sum = sum + magnitude - i - 1
  }

  return(enumeration)
}


Triangulate_test2 <- function(graph, elim.order){
  dag.graph <- igraph.from.graphNEL(graph, weight=FALSE)
  for(i in 1:length(elim.order)){
    # print(c("on ", i))

    node <- elim.order[i]
    neighbors <- names(neighbors(dag.graph, node, mode="all"))
    neighbors_length <- length(neighbors)
    if(neighbors_length >= 2){
      node_pairs <- generate_pairs(neighbors_length)

      for(j in 1:nrow(node_pairs)){
        n1 <- neighbors[node_pairs[j, 1]]
        n2 <- neighbors[node_pairs[j, 2]]
        if (which(elim.order==n1)>i && which(elim.order==n2)>i){
          if(get.edge.ids(dag.graph, c(n1, n2)) == 0){


            dag.graph <- add_edges(dag.graph, c(n1, n2))
          }
        }
      }
    }
  }
  # print("simplifying")
  # dag.graph <- simplify(dag.graph, remove.loops = FALSE)
  dag.tri <- igraph.to.graphNEL(dag.graph)
  return(dag.tri)

}



#Note that this uses adjacency matrix implementation, which could present problems for massive datasets

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
  dag_matrix <- dag_matrix != 0

  dag.graph <- igraph.from.graphNEL(graph, weight=FALSE)

  for(i in 1:length(elim.order)){
    # get neighbors that are after ith node in elim.order
    # get neighbors
    neighbors <- elim.order[dag_matrix[i, ]]

    # keep only neighbors after ith node in elim.order
    neighbors <- neighbors[match(neighbors, elim.order) > i]


    if(length(neighbors) >= 2){


      for(n1 in 1:(length(neighbors)-1)){
        for(n2 in (n1+1):length(neighbors)){
          first <- which(neighbors[n1] == elim.order)
          second <- which(neighbors[n2] == elim.order)

          if(!dag_matrix[first, second]){


            dag.graph <- add_edges(dag.graph, c(neighbors[n1], neighbors[n2]))
            dag_matrix[first, second] <- TRUE
          }
        }
      }
    }

  }

  dag.tri <- igraph.to.graphNEL(dag.graph)
  return(dag.tri)
}



