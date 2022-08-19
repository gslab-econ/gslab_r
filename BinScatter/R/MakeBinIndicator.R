#' Create bin indicators and bin centers for `x_var`
#' 
#' @param var Var
#' @param data Data
#' @param nBins Number of bins
#' @param binType Type of bin
#' @param intercept Boolean for intercept
#' @param min_obs Minimum number of observations per bin
#' @param is_xvar Boolean for whether `var` is `x_var`
#' @import "data.table"
#' @import mltools
#'

MakeBinIndicator <- function(var, data, nBins, binType, intercept, min_obs, is_xvar = FALSE) {
  
  if (binType == "uniform") {
    tempBinType <- "explicit"
  } else if (binType == "quantile") {
    tempBinType <- "quantile"
  } else {
    stop(sprintf("binType for %s must be uniform or quantile", var))
  }
  
  # Create a data frame of bin indicators
  vals           <- data[, eval(var)] %>% as.numeric()
  bins           <- bin_data(vals, bins = nBins, binType = tempBinType) %>% factor(ordered = FALSE)
  if (nBins != length(unique(bins))) {stop("ISSUE 1")}
  if (length(table(bins)) != length(table(bins)[table(bins) >= min_obs])) {stop("ISSUE 2")}
  df_bins        <- data.table(bins) %>% one_hot()
  names(df_bins) <- paste0(var, "_bin_", c(1:nBins))
  
  if (intercept) {
    df_bins        <- df_bins[, 2:nBins]
    names(df_bins) <- paste0(var, "_bin_", c(2:nBins))
  }
  
  # Return additionally bin centers for the `x_var`
  if (is_xvar) {
    if (binType == "uniform") {
      bin_center <- sapply(unique(bins), GetMidpoint) %>% sort()
    } else if (binType == "quantile") {
      bin_center <- sapply(unique(bins), GetMeanVal, vals) %>% sort()
    }
    return(list(indicator = df_bins, center = bin_center))
  } else {
    return(df_bins)
  }
}
