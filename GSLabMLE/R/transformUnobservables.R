#' Transform unobservables in the model.
#' @description This function takes as input a vector of parameters, a \code{MLEData} data, and 
#' a list of i.i.d. draws of unobservables, and returns a list of transformed unobservables.
#' In \code{MLEModel}, this method simply passes through the raw unobservables. Implementing 
#' subclasses typically transform the unobservables as a function of parameters and data.
#' For models with no unobservables, this method can simply return an empty list.
#' @param .self A \code{MLEModel} object.
#' @param param A vector of parameters at which to evaluate unobservables.
#' @param data A \code{MLEData} object.
#' @param raw_unobs A list of unobservable with one field for each unobservable in the model and
#' field names matching unobservable names.
#' For group-level unobservables, each field of \code{raw_unobs} contains standard normal draws at 
#' the group level, with values replicated across draws within groups.
#' For individual-level unobservables, \code{raw_unobs} contains i.i.d. standard normal draws 
#' across observations. The transformation function must not exhibit dependencies across groups, 
#' nor, in the case of group-level unobservables, across observations.
#' 
transformUnobservables <- function(.self, param, data, raw_unobs) {
    return (raw_unobs)
}