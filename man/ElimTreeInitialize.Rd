% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C1_ElimTreeInitialize.R
\name{ElimTreeInitialize}
\alias{ElimTreeInitialize}
\title{Initialize the elimination tree}
\usage{
ElimTreeInitialize(tree, dag, model, node.sets, node.class)
}
\arguments{
\item{tree}{a \code{graphNEL} object of the elimination tree}

\item{dag}{a \code{graphNEL} object of the Bayesian network}

\item{model}{a \code{list} of local models built from \code{\link{LocalModelCompile}} function}

\item{node.sets}{a \code{list} of cluster sets obtained from \code{\link{ClusterTreeCompile}} function}

\item{node.class}{a named \code{vector} of \code{logical} values, \code{TRUE} if node 
is discrete, \code{FASLE} if otherwise}
}
\value{
\code{\linkS4class{ClusterTree}} object with the local models incorporated
}
\description{
Initialize the elimination tree with the local models
}
\details{
Initialize the elimination tree with the local models
}
\examples{

data(liver)
cst <- ClusterTreeCompile(dag=liver$dag, node.class=liver$node.class)
models <- LocalModelCompile(data=liver$data, dag=liver$dag, node.class=liver$node.class)
tree.init <- ElimTreeInitialize(tree=cst$tree.graph, 
                                dag=cst$dag, 
                                model=models, 
                                node.sets=cst$cluster.sets, 
                                node.class=cst$node.class)

}
\references{
Cowell, R. G. (2005). Local propagation in conditional Gaussian Bayesian networks. 
Journal of Machine Learning Research, 6(Sep), 1517-1550.
}
\seealso{
The functions \code{\link{ClusterTreeCompile}} and \code{\link{LocalModelCompile}} provide necessary
objects to obtain \code{\linkS4class{ClusterTree}} object by initializing the elimination tree through this function.
}
\author{
Han Yu
}
