% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLEConstraints.R
\docType{class}
\name{MLEConstraints-class}
\alias{MLEConstraints-class}
\alias{MLEConstraints}
\title{A Reference Class that Defines Constraints for a Maximum Likelihood Model.}
\description{
The general optimization problem solved by optim:L-BFGS-B is:
\deqn{min f(x), s.t.	lower<=x<=upper}
When \eqn{lower=upper} the constraint becomes an equality constraint. This class defines these constraints.
}
\section{Fields}{

\describe{
\item{\code{lower}}{The lower bounds on the parameters.}

\item{\code{upper}}{The upper bounds on the parameters.}

\item{\code{paramlist}}{A vector of parameter names.}

\item{\code{nparam}}{The number of parameters.}

\item{\code{indices}}{A list giving the index of each parameter.}
}}

\section{Methods}{

\describe{
\item{\code{isConsistent(param, tolerance = 1e-04)}}{\subsection{Description}{
        Determines whether parameters satisfy constraints.}

        \subsection{Parameters}{
        \code{param}: The parameter vector at which to check against constraints.

        \code{xtol}: A scalar controlling the tolerance of tests. Default is 1e-4.}

        \subsection{Return}{
        A logical scalar indicating whether or not parameters satisfy constraints.}}

\item{\code{jacobianOfConstraints(param)}}{\subsection{Description}{
        Returns the jacobian of constraints.}

        \subsection{Parameters}{
        \code{param}: The parameter vector at which to evaluate Jacobian matrix.

        \code{xtol}: A scalar controlling the step size of numerical derivatives.}

        \subsection{Return}{
        A list that contains the Jacobians of different constraints.}}

\item{\code{removeBound(constr_paramlist)}}{\subsection{Description}{
        Remove upper and lower bound constraints of parameters.}

        \subsection{Parameters}{
        \code{constr_paramlist}: A subset of \code{paramlist} containing the parameters whose
        upper and lower bounds are to be removed.}}

\item{\code{setFixedBound(constr_paramlist, bounds)}}{\subsection{Description}{
        Update the fixed value constraints of parameters.}

        \subsection{Parameters}{
        \code{constr_paramlist}: A subset of \code{paramlist} containing the parameters to be constrained.

        \code{bounds}: A vector with the same length as \code{constr_paramlist} that defines
        the fixed constraints for all parameters in \code{constr_paramlist}.
}}

\item{\code{setLowerBound(constr_paramlist, bounds)}}{\subsection{Description}{
        Update the lower bound constraints of parameters.}

        \subsection{Parameters}{
        \code{constr_paramlist}: A subset of \code{paramlist} containing the parameters to be constrained.

        \code{bounds}: A vector with the same length as \code{constr_paramlist} that defines
        the lower bounds for all parameters in \code{constr_paramlist}.
}}

\item{\code{setUpperBound(constr_paramlist, bounds)}}{\subsection{Description}{
        Update the upper bound constraints of parameters.}

        \subsection{Parameters}{
        \code{constr_paramlist}: A subset of \code{paramlist} containing the parameters to be constrained.

        \code{bounds}: A vector with the same length as \code{constr_paramlist} that defines
        the upper bounds for all parameters in \code{constr_paramlist}.
}}
}}

