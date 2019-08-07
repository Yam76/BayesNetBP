#' Initialize a ClusterTree object
#'
#' Initialize a ClusterTree object
#'
#' @details A wrapper function to initialize a \code{\linkS4class{ClusterTree}} object. It combines
#' the functions of \code{\link{ClusterTreeCompile}}, \code{\link{LocalModelCompile}},
#' \code{\link{ElimTreeInitialize}} and \code{\link{Propagate}}, thus initialize the \code{\linkS4class{ClusterTree}}
#' object in a single step.
#'
#' @param dag a \code{graphNEL} object of the Bayesian network
#' @param data a \code{data.frame} object
#' @param node.class a named \code{vector} of \code{logical} values, \code{TRUE} if node
#' is discrete, \code{FASLE} if otherwise
#' @param propagate \code{logical} \code{TRUE} if the discrete part of the \code{\linkS4class{ClusterTree}}
#' to be propagated
#'
#' @return \code{\linkS4class{ClusterTree}} object
#'
#' @author Han Yu
#'
#' @references Cowell, R. G. (2005). Local propagation in conditional Gaussian Bayesian networks.
#' Journal of Machine Learning Research, 6(Sep), 1517-1550.
#'
#' @examples
#' data(liver)
#' tree.init.p <- Initializer(dag=liver$dag, data=liver$data,
#'                            node.class=liver$node.class,
#'                            propagate = TRUE)
#' @seealso \code{\link{ClusterTreeCompile}}, \code{\link{LocalModelCompile}}, \code{\link{ElimTreeInitialize}},
#' \code{\link{Propagate}}
#'
#' @export

Initializer <- function(dag, data, node.class, propagate = TRUE){
  cst <- ClusterTreeCompile(dag=dag, node.class=node.class)
  models <- LocalModelCompile(data=data, dag=dag, node.class=node.class)
  tree.init <- ElimTreeInitialize(tree=cst$tree.graph,
                                  dag=cst$dag,
                                  model=models,
                                  node.sets=cst$cluster.sets,
                                  node.class=cst$node.class)
  if(propagate) {
    tree.init <- Propagate(tree.init)
  }
  return(tree.init)
}

MultiInitializer <- function(dag, data, node.class, propagate = TRUE){
  init_list <- Separator(dag, data, node.class)
  final_list <- vector("list", length(init_list))
  for(i in 1:length(init_list)){
    final_list[[i]] <- Initializer(init_list[[i]]$dag, init_list[[i]]$data, init_list[[i]]$node.class, propagate)
  }
  return(Combiner(final_list))

}

# returns a list with dag, data, and node.class for each
#' @importFrom igraph igraph.from.graphNEL
Separator <- function(dag, data, node.class){

  dag_graph <- igraph.from.graphNEL(dag, unlist.attrs = FALSE)
  decomposed_graph <- decompose.graph(dag_graph)

  separated_list <- vector("list", length(decomposed_graph))
  for(i in 1:length(decomposed_graph)){
    node_names <- names(V(decomposed_graph[[i]]))
    node_data <- data[node_names]
    subset_node_class <- node.class[node_names]
    separated_list[[i]] <- list(dag = as_graphnel(decomposed_graph[[i]]), data = node_data, node.class = subset_node_class)
  }

  return(separated_list)
}

combine_graphNEL <- function(graph1, graph2){
  return(as_graphnel(
    simplify(
      union(igraph.from.graphNEL(graph1, unlist.attrs = FALSE), igraph.from.graphNEL(graph2, unlist.attrs = FALSE))
       , remove.loops = FALSE, edge.attr.comb = igraph_opt("ignore")
       )
    )
  )
}

# basically end wrapper for Initializer
Combiner <- function(tree_list){
  if(length(tree_list) == 1){
    return(tree_list[[1]])
  }

  total_tree <- tree_list[[1]]
  tree_slots <- slotNames(total_tree)
  for(i in 2:length(tree_list)){
    for(j in 1:length(tree_slots)){
      if(tree_slots[j] == "graph"){
        slot(total_tree, tree_slots[j])[["dag"]] <- combine_graphNEL(slot(total_tree, tree_slots[j])[["dag"]], slot(tree_list[[i]], tree_slots[j])[["dag"]])
        slot(total_tree, tree_slots[j])[["tree"]] <- combine_graphNEL(slot(total_tree, tree_slots[j])[["tree"]], slot(tree_list[[i]], tree_slots[j])[["tree"]])
      }
      else if(tree_slots[j] == "propagated"){
        slot(total_tree, tree_slots[j]) <- slot(total_tree, tree_slots[j]) & slot(tree_list[[i]], tree_slots[j])
      }
      else{
        slot(total_tree, tree_slots[j]) <- c(slot(total_tree, tree_slots[j]), slot(tree_list[[i]], tree_slots[j]))
      }
    }
  }

  return(total_tree)
}




