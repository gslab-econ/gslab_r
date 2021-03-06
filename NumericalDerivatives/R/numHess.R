#' Numerically Calculate Hessian matrix
#' @description This function numerically calculates the Hessian matrix of a given scalar-valued
#' function evaluated at given values of arguments.
#' @param func A scalar-valued function on which Hessian matrix is calculated.
#' @param x0 A vector of argument values of at which the Hessian matrix is evaluated.
#' @param xTol The tolerance, where a smaller \code{xTol} corresponds to increased accuracy
#' of the numerical procedure.
#' @param ind_rowvar Index of rows where the Hessian matrix is calculated. Default is all rows.
#' @param ind_colvar Index of columns where the Hessian matrix is calculated. Default is all columns.
#' @examples
#' example <- function(x) sqrt(prod(x))
#' numHess(example, c(1, 2, 3, 4, 5), 1e-4, c(1, 3, 5), c(2, 4))
#' @return A matrix with the number of rows equal to the length of \code{ind_rowvar}, and the
#' number of columns equal to the length of \code{ind_colvar}.
#' @export
#' 
numHess <- function(func, x0, xTol, ind_rowvar = 1:length(x0), ind_colvar = 1:length(x0)) {
    f0 <- func(x0)
    if (any(is.na(f0))) {
        stop("Invalid function or argument input")
    }
    if (length(f0) > 1) {
        stop("Only a scalar-valued function is allowed")
    }
    n  <- length(x0)
    if (any(ind_rowvar > n) | any(ind_colvar > n)) {
        stop("Index exceeds the length of arguments")
    }
    f  <- function(row, col, func, f0, x0, n, xTol) {
        increment_i <- rep(0, n)
        increment_j <- rep(0, n)
        increment_i[row] <- max(x0[row] * xTol, xTol)
        increment_j[col] <- max(x0[col] * xTol, xTol)
        increment_ij <- increment_i + increment_j
        f_i  <- func(x0 + increment_i)
        f_j  <- func(x0 + increment_j)
        f_ij <- func(x0 + increment_ij)
        return ((f_ij + f0 - f_i - f_j) / (increment_i[row] * increment_j[col]))   
    }
    f_col <- function(col, func, f0, x0, n, xTol, ind_rowvar, f) {
        sapply(ind_rowvar,
               function(row, func, f0, x0, n, xTol, f)
                   f(row, col, func, f0, x0, n, xTol),
               func, f0, x0, n, xTol, f)
    }
    Hess <- sapply(ind_colvar, f_col, func, f0, x0, n, xTol, ind_rowvar, f)
    return (matrix(Hess, nrow = length(ind_rowvar)))
}
