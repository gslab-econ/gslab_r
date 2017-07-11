#' Numerically Calculate Jacob Matrix
#' @description The function numerically calculates the Jacob matrix of a given function 
#' evaluated at given values of arguments.
#' @param func The function on which Jacob matrix is calculated.
#' @param x0 The values of arguments at which the Jacob matrix is evaluated.
#' @param xTol The tolerance, where a small \code{xTol} corresponds to increased accuracy
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
    Jacobian <- matrix(0, noutput, nparam)
    for (j in 1:nparam) {
        increment    <- array(0, paramdim)
        increment[j] <- max(abs(x0[j]) * xTol, xTol)
        x1 <- x0 + increment
        f1 <- func(x1)
        Jacobian[, j] <- (f1 - f0) / increment[j]  
    }
    return (Jacobian)
}
