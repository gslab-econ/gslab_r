#' Numerically Calculate Jacobian Matrix
#' @description The function numerically calculates the Jacobian matrix of a given function 
#' evaluated at given values of arguments.
#' @param func A scalar- or vector-valued function on which Jacobian matrix is calculated.
#' @param x0 A vector or a matrix of argument values at which the Jacobian matrix is evaluated.
#' @param xTol The tolerance, where a smaller \code{xTol} corresponds to increased accuracy
#' of the numerical procedure.
#' @examples
#' example <- function(x) x[1]^2 + x[2]^2
#' numJacob(example, c(1, 2), 1e-6)
#' @return A matrix with the number of rows equal to the length of output of \code{func}, and the
#' number of columns equal to the length of \code{x0}.
#' @export
#' 
numJacob <- function(func, x0, xTol) {
    f0 <- func(x0)
    if (any(is.na(f0))) {
        stop("Invalid function or argument input")
    }
    paramdim <- dim(x0)
    if (is.null(paramdim)) {
        paramdim <- length(x0)
    }
    f <- function(j, func, f0, x0, paramdim, xTol) {
        increment    <- array(0, paramdim)
        increment[j] <- max(abs(x0[j]) * xTol, xTol)
        f1 <- func(x0 + increment)
        return ((f1 - f0) / increment[j])
    }
    Jacobian <- sapply(1:length(x0), f, func, f0, x0, paramdim, xTol)
    return (matrix(Jacobian, ncol = length(x0)))
}
