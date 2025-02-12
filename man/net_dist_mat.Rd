% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_dist_mat.R
\name{net_dist_mat}
\alias{net_dist_mat}
\title{Generate distance matrix from gl}
\usage{
net_dist_mat(gl)
}
\arguments{
\item{gl}{A list of graphs.}
}
\value{
An n x n x 3 matrix, where n is the number of graphs in gl
the first slice \link{,,1} contains the weight difference of shared edges
the second slice \link{,,2} contains the number of edge-changes among shared scales
the third slice \link{,,3} contains the number of un-shared scales
}
\description{
Generate distance matrix from gl
}
\examples{
data('simpleGs')

dmat <- net_dist_mat(simpleGs)

dmat ##view the difference matrices
}
