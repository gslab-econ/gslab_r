#' Numerically Calculate Jacobian Matrix
#' @description The function numerically calculates the Jacobian matrix of a given function 
#' evaluated at given values of arguments.
#' @param func The function on which Jacobian matrix is calculated.
#' @param x0 The values of arguments at which the Jacobian matrix is evaluated.
#' @param xTol The tolerance, where a smaller \code{xTol} corresponds to increased accuracy
#' of the numerical procedure.
#' @examples
#' example <- function(x) x[1]^2 + x[2]^2
#' numJacob(example, c(1, 2), 1e-6)
#' @export
#' 
numJacob <- function(func, x0, xTol) {
    f0       <- func(x0)
    noutput  <- length(f0)
    nparam   <- length(x0)
    paramdim <- dim(x0)
    if (is.null(paramdim)) {
        paramdim <- nparam
    }
    f <- function(j) {
        increment    <- array(0, paramdim)
        increment[j] <- max(abs(x0[j]) * xTol, xTol)
        f1 <- func(x0 + increment)
        return ((f1 - f0) / increment[j])
    }
    Jacobian <- sapply(1:nparam, f)
    return (Jacobian)
}