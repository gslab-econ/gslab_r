#' Compute a derived parameter
#' @description This method takes a vector of parameters, constants and the name of a derived
#' parameter, and returns the value of the derived parameter. In \code{MLEModel}, this method simply
#' returns an empty value. Implementing subclasses typically populates this method to handle all
#' derived parameters listed in the \code{dparamlist} field of the model using \code{switch} protocol.
#' @param .self An \code{MLEModel} object.
#' @param param A vector of parameters at which to calculate the derived parameters.
#' @param dparamname The name of the derived parameter.
#' @param constants A list of constants used to calculate the derived parameters.
#' @export
#' 
derivedParam <- function(.self, param, dparamname, constants) {
    switch(dparamname,
           NULL = numeric(0))
}

