% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelEstimationOptions.R
\docType{class}
\name{ModelEstimationOptions-class}
\alias{ModelEstimationOptions-class}
\alias{ModelEstimationOptions}
\title{A Reference Class that Defines Options for the \code{Estimate} Method of \code{Model}}
\description{
A Reference Class that Defines Options for the \code{Estimate} Method of \code{Model}
}
\section{Fields}{

\describe{
\item{\code{outlev}}{The output level of the Knitro optimization. A larger value indicates more verbose
output. The default 0 suppresses all output information.}

\item{\code{startparam}}{Starting paramter vector.}

\item{\code{hesstol}}{Numerical step for computing the Hessian.}

\item{\code{knitrotxt}}{A list of additional options to be passed to Knitro. See more at "help knitro".}
}}

