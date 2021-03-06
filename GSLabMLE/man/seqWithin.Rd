% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{seqWithin}
\alias{seqWithin}
\title{Sort group variable and create index within each group}
\usage{
seqWithin(group)
}
\arguments{
\item{group}{A matrix or a vector to be sorted.}
}
\value{
A list with two fields: \code{sorted_group}, the sorted group matrix,
and \code{indices}, a single-column matrix of the group indices.
}
\description{
The function takes as input a group matrix. A group is defined by an entire
row in the group matrix. The function sort the group matrix by first column, then second, ...
It also return a single-column matrix that indicates the index of each row within each group.
}
\examples{
seqWithin(matrix(c(2, 3, 1, 1, 3, 3, 2, 2, 3, 3, 1, 1, 3, 3), 7, byrow = TRUE))
}
