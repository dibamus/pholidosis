% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morpho_matrix.R
\name{morpho_matrix}
\alias{morpho_matrix}
\title{Generate a morphological character matrix from a list of graphs}
\usage{
morpho_matrix(gl, custom = FALSE)
}
\arguments{
\item{gl}{A list of graphs}

\item{custom}{A boolean; whether the matrix contains values other than 1, 1.5, and 2}
}
\value{
A n*e matrix, where n is the number of graphs in gl,
and e is the total number of unique edges in gl.
An element, \link{n1,e1} is the weight of edge e1 in graph n1.
}
\description{
Generate a morphological character matrix from a list of graphs
}
\examples{
data("simpleGs")
mat <- morpho_matrix(simpleGs)
mat

}
