#' @importFrom igraph as.undirected

Moralize <- function(graph){
  Moralize_test3(graph)
}

Moralize_orig <- function(graph){
  # graph <- emission1000$dag
  dag.graph <- igraph.from.graphNEL(graph, weight=FALSE)
  und.graph <- as.undirected(dag.graph, mode = "collapse")
  # iterate over all nodes
  nds <- graph::nodes(graph)

  for (i in 1:length(nds)) {
    # i <- 1
    node <- nds[i]
    pa <- names(neighbors(dag.graph, node, mode="in"))
    n.pa <- length(pa)

    if (n.pa>=2) {
      for (k1 in 1:(n.pa - 1)) {
        for (k2 in (k1+1):n.pa) {
          if(!are_adjacent(und.graph, pa[k1], pa[k2])){
            und.graph <- add_edges(und.graph, c(pa[k1],pa[k2]))
          }
        }
      }
    }
  }

  nel.mor <- igraph.to.graphNEL(und.graph)
  # x11(); plot(nel.mor)
  return(nel.mor)

}

#' @importFrom graph inEdges
#' @importFrom igraph simplify
Moralize_test3 <- function(graph){

  dag_nodes <- nodes(graph)

  und.graph <- as.undirected(igraph.from.graphNEL(graph, weight=FALSE), mode = "collapse")

  for(i in 1:length(dag_nodes)){
    parents <- inEdges(dag_nodes[i], graph)[[dag_nodes[i]]]
    parents_length <- length(parents)
    if(parents_length >= 2){
      for(p1 in 1:(parents_length-1)){
        for(p2 in (p1+1):(parents_length)){

            und.graph <- add_edges(und.graph, c(parents[p1], parents[p2])) # are_adjacent is expensive, so we don't use that.

        }
      }
    }

  }
  und.graph <- simplify(und.graph, remove.loops=FALSE)
  nel.mor <- igraph.to.graphNEL(und.graph)
  return(nel.mor)

}

Moralize_test2 <- function(graph){
  dag_adj_list <- edgeL(graph)
  dag_nodes <- nodes(graph)

  und.graph <- as.undirected(igraph.from.graphNEL(graph, weight=FALSE), mode = "collapse")

  for(i in 1:length(dag_nodes)){
    parents <- inEdges(dag_nodes[i], graph)[[dag_nodes[i]]]
    parents_length <- length(parents)

    print(parents)

    if(parents_length >= 2){
      for(n1 in 1:(parents_length-1)){
        for(n2 in (n1+1):parents_length){
          # names of the 2 parents
          parent1 <- parents[n1]
          parent2 <- parents[n2]

          # indices of 2 parents
          first <- which(dag_nodes == parent1)
          second <- which(dag_nodes == parent2)

          if( !(second %in% dag_adj_list$parent1$edges) && !(first %in% dag_adj_list$parent2$edges && !are_adjacent(und.graph, parent1, parent2)) ){
            print(c("Adding edge between ", parent1, " ", parent2))
            und.graph <- add_edges(und.graph, c(parent1, parent2))
          }
        }
      }
    }

  }

  nel.mor <- igraph.to.graphNEL(und.graph)
  return(nel.mor)

}

Moralize_test <- function(graph){
  dag_matrix <- as(graph, "matrix") == 1
  nodes <- graph@nodes

  und.graph <- as.undirected(igraph.from.graphNEL(graph, weight=FALSE), mode = "collapse")

  for(i in 1:length(nodes)){

    parents <- nodes[dag_matrix[, i]]
    parents_length <- length(parents)

    # neighbors <- und_matrix[i,]
    # neighbors <- nodes[neighbors]
    # neighbors_length <- length(neighbors)

    if(parents_length >= 2){
      for(n1 in 1:(parents_length-1)){
        for(n2 in (n1+1):parents_length){
          # names of the 2 parents
          parent1 <- parents[n1]
          parent2 <- parents[n2]

          # indices of the 2 neighbors in nodes
          first <- which(nodes == parent1)
          second <- which(nodes == parent2)

          if(!dag_matrix[first, second] && !dag_matrix[second, first]){
            und.graph <- add_edges(und.graph, c(parent1, parent2))
            # dag_matrix[first, second] <- TRUE we only consider the ORIGINAL graph when Moralizing
            # dag_matrix[second, first] <- TRUE
          }

        }
      }
    }

  }
  nel.mor <- igraph.to.graphNEL(und.graph)
  return(nel.mor)

}




