% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/G1_PlotCGBN.R
\name{PlotCGBN}
\alias{PlotCGBN}
\title{Plot the Bayesian network}
\usage{
PlotCGBN(tree.1, tree.2, fontsize = NULL, pbar = FALSE,
  plotting = TRUE)
}
\arguments{
\item{tree.1}{a \code{\linkS4class{ClusterTree}}}

\item{tree.2}{a \code{\linkS4class{ClusterTree}}}

\item{fontsize}{font size for the node labels}

\item{pbar}{\code{logical(1)} whether to show progress bar}

\item{plotting}{\code{logical(1)} whether to output plot}
}
\value{
a plot of Bayesian network

a \code{vector} of signed symmetric Kullback-Leibler divergence
}
\description{
Plot and compare two Bayesian networks with different evidence(s) absorbed and propagated.
}
\details{
Network visualization of the node-specific differences between Bayesian Networks 
with the same topology, but evidence that has been absorbed and propagated.  The change of 
marginal distribution of each node is measured by signed and symmetric Kullback-Leibler 
divergence.  The sign indicates the direction of change, with \code{tree.1} considered as the baseline.  
The magnitude of the change is reflected by the value.  Nodes that are white are d-separated 
from the evidence.
}
\examples{
\dontrun{
data(toytree)
tree.post <- AbsorbEvidence(toytree, c("Nr1i3"), list(1))
PlotCGBN(tree.1=toytree, tree.2=tree.post)
}
}
\references{
Cowell, R. G. (2005). Local propagation in conditional Gaussian Bayesian networks. 
Journal of Machine Learning Research, 6(Sep), 1517-1550.
}
\author{
Han Yu
}
