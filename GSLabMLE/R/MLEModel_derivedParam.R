#' @include MLEModel.R
MLEModel$methods(
    derivedParam = function(param, dparamname, constants) {
        "\\subsection{Description}{
        Compute a derived parameter.\n
        This method takes a vector of parameters, constants and the name of a derived parameter,
        and returns the value of the derived parameter. In \\code{MLEModel}, this method simply
        returns an empty value. Implementing subclasses typically populates this method to handle
        all derived parameters listed in the \\code{dparamlist} field of the model using
        \\code{switch} protocol.}\n
        \\subsection{Parameters}{
        \\code{param}: A vector of parameters at which to calculate the derived parameters.\n
        \\code{dparamname}: The name of the derived parameter.\n
        \\code{constants}: A list of constants used to calculate the derived parameters.}\n
        \\subsection{Return}{
        The deried parameter.}"
        switch(dparamname,
               NULL = numeric(0))
    }
)
