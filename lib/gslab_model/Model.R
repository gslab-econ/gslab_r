model <- setRefClass("model",
                     fields = list(paramlist          = "character",
                                   default_startparam = "numeric",
                                   lhslist            = "character",
                                   rhslist            = "character",
                                   nparam             = "numeric")
)