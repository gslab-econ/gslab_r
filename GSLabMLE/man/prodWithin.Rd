% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{prodWithin}
\alias{prodWithin}
\title{Compute group product}
\usage{
prodWithin(value, group)
}
\arguments{
\item{value}{A matrix or a vector.}

\item{group}{A matrix or a vector. The number of rows is the same as \code{value}.}
}
\value{
A list with two fields: \code{group}, a matrix that contains the unique groups,
and \code{value}, a matrix that contains the products by group.
}
\description{
The function takes as input a value matrix and a group matrix. A group is defined by an entire
row in the group matrix. For each column of the value matrix, the function computes the product
of all values within each unique group.
}
