#' Numerically Calculate Hessian matrix
#' @description This function numerically calculates the Hessian matrix of a given function
#' evaluated at given values of arguments.
#' @param func The function on which Hessian matrix is calculated.
#' @param x0 The values of arguments at which the Hessian matrix is evaluated.
#' @param xTol The tolerance, where a smaller \code{xTol} corresponds to increased accuracy
#' of the numerical procedure.
#' @param ind_rowvar Index of rows where the Hessian matrix is calculated. Default is all rows.
#' @param ind_colvar Index of columns where the Hessian matrix is calculated. Default is all columns.
#' @examples
#' example <- function(x) sqrt(prod(x))
#' numHess(example, c(1, 2, 3, 4, 5), 1e-4, c(1, 3, 5), c(2, 4))
#' @export
#' 
numHess <- function(func, x0, xTol, ind_rowvar = 1:length(x0), ind_colvar = 1:length(x0)) {
    f0 <- func(x0)
    f  <- function(row, col) {
        increment_i <- rep(0, length(x0))
        increment_j <- rep(0, length(x0))
        increment_i[row] <- max(x0[row] * xTol, xTol)
        increment_j[col] <- max(x0[col] * xTol, xTol)
        increment_ij <- increment_i + increment_j
        f_i  <- func(x0 + increment_i)
        f_j  <- func(x0 + increment_j)
        f_ij <- func(x0 + increment_ij)
        return ((f_ij + f0 - f_i - f_j) / (increment_i[row] * increment_j[col]))   
    }
    Hess <- sapply(ind_colvar, function(col) sapply(ind_rowvar, function(row) f(row, col)))
    return (Hess)
}
