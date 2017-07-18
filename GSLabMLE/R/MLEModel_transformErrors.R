#' @include MLEModel.R
MLEModel$methods(
    transformErrors = function(param, data, raw_error) {
        "\\subsection{Description}{
        Transform errors in the model.\n
        This function takes as input a vector of parameters, an \\code{MLEData} object, and a list
        of i.i.d. draws of errors, and returns a list of transformed errors. 
        In \\code{MLEModel}, this method simply passes through the raw errors.
        Implementing subclasses typically transform the errors as a function of parameters and data.
        For models with no errors, this method can simply return an empty list.}\n
        \\subsection{Parameters}{
        \\code{param}: A vector of parameters at which to evaluate errors.\n
        \\code{data}: An \\code{MLEData} object\n
        \\code{raw_error}: A list of errors with one field for each error in the model and field names 
         matching error names. The raw errors are drawn i.i.d. across observations with distributions
        defined by the \\code{error_distributions} field of the model. For vector-valued errors
        (dimension > 1 as defined by the \\code{error_dimensions} field of the model),
        the draws are also i.i.d. across elements of the vector.}\n
        \\subsection{Return}{
        A list of transformed errors.}"
        return (raw_error)
    }
)