#' Transform errors in the model.
#' @description This function takes as input a vector of parameters, a \code{MLEData} data, and 
#' a list of i.i.d. draws of errors, and returns a list of transformed errors.
#' In \code{MLEModel}, this method simply passes through the raw errors. Implementing subclasses
#' typically transform the errors as a function of parameters and data.
#' For models with no errors, this method can simply return an empty list.
#' @param .self A \code{MLEModel} object.
#' @param param A vector of parameters at which to evaluate errors.
#' @param data A \code{MLEData} object.
#' @param raw_error A list of errors with one field for each error in the model and field names 
#' matching error names. The raw errors are drawn i.i.d. across observations with distributions
#' defined by the \code{error_distributions} field of the model. For vector-valued errors
#' (dimension > 1 as defined by the \code{error_dimensions} field of the model),
#' the draws are also i.i.d. across elements of the vector.
#' 
transformErrors <- function(.self, param, data, raw_error) {
    return (raw_error)
}