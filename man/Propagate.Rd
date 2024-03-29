% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D3_Propagate.R
\name{Propagate}
\alias{Propagate}
\title{Propagate the cluster tree}
\usage{
Propagate(tree, targets = NA)
}
\arguments{
\item{tree}{an initialized \code{\linkS4class{ClusterTree}} object}
}
\value{
a \code{\linkS4class{ClusterTree}} object
}
\description{
This function propagates the discrete compartment of a \code{\linkS4class{ClusterTree}} object.
}
\details{
The discrete compartment must be propagted to get the joint distributions
of discrete variables in each discrete clusters. A \code{\linkS4class{ClusterTree}} object must be propagated
before absorbing evidence and making queries.
}
\examples{

data(liver)
tree.init <- Initializer(dag=liver$dag, data=liver$data,
                         node.class=liver$node.class,
                         propagate = FALSE)
tree.init@propagated
tree.init.p <- Propagate(tree.init)
tree.init.p@propagated

}
