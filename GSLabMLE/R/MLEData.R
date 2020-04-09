#' A Reference Class that Defines Data Used in a Maximum Likelihood Model
#' @import methods
#' @importClassesFrom GSLabModel ModelData
#' @inheritSection GSLabModel::ModelData Fields
#' @inheritSection GSLabModel::ModelData Methods
#' @export MLEData
#' @exportClass MLEData
MLEData <- setRefClass(Class    = "MLEData",
                       contains = "ModelData")