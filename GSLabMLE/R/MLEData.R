#' A Reference Class that Defines Data Used in a Maximum Likelihoo Model.
#' @export MLEData
#' @exportClass MLEData
#' @importClassesFrom GSLabModel ModelData
#' 
MLEData <- setRefClass(Class    = "MLEData",
                       contains = "ModelData")