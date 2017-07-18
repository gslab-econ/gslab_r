#' @include MLEModel.R
MLEModel$methods(
    transformUnobservables = function(param, data, raw_unobs) {
        "\\subsection{Description}{
        Transform unobservables in the model. \n
        This function takes as input a vector of parameters, an \\code{MLEData} object, and a list
        of i.i.d. draws of unobservables, and returns a list of transformed unobservables. 
        In \\code{MLEModel}, this method simply passes through the raw unobservables.
        Implementing subclasses typically transform the unobservables as a function of parameters
        and data. For models with no unobservables, this method can simply return an empty list.}\n
        \\subsection{Parameters}{
        \\code{param}: A vector of parameters at which to evaluate unobservables.\n
        \\code{data}: An \\code{MLEData} object\n
        \\code{raw_unobs}: A list of unobservables with one field for each unobservable in the model
        and field names matching unobservables names.\n
        For group-level unobservables, each field of \\code{raw_unobs} contains standard normal
        draws at the group level, with values replicated across draws within groups.\n
        For individual-level unobservables, \\code{raw_unobs} contains i.i.d. standard normal
        draws across observations.\n
        The transformation function must not exhibit dependencies across groups, nor, in the case of
        group-level unobservables, across observations.}\n
        \\subsection{Return}{
        A list of transformed unobservables.}"
        return (raw_unobs)
    }
)