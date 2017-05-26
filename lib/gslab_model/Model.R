model <- setRefClass("model",
                     fields = list(paramlist  = "character",
                                   startparam = "numeric",
                                   lhslist    = "character",
                                   rhslist    = "character",
                                   nparam     = "numeric")
)