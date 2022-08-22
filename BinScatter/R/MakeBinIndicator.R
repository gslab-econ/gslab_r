#' Create bin indicators and bin centers for `x_var`
#' 
#' @param var Var
#' @param data Data
#' @param n_bins Number of bins
#' @param bin_type Type of bin
#' @param intercept Boolean for intercept
#' @param min_obs Minimum number of observations per bin
#' @param is_xvar Boolean for whether `var` is `x_var`
#' @importFrom "data.table" "data.table"
#' @importFrom mltools one_hot bin_data


MakeBinIndicator <- function(var, data, n_bins, bin_type, min_obs, intercept, is_xvar = FALSE) {
  
  if (bin_type == "uniform") {
    temp_bin_type <- "explicit"
  } else if (bin_type == "quantile") {
    temp_bin_type <- "quantile"
  } else {
    stop(sprintf("bin_type for %s must be uniform or quantile", var))
  }
  
  # Create a data frame of bin indicators
  vals           <- data[, eval(var)] %>% as.numeric()
  bins           <- bin_data(vals, bins = n_bins, binType = temp_bin_type) %>% factor(ordered = FALSE)
  if (n_bins != length(table(bins)[table(bins) >= min_obs])) {
    stop(sprintf("One or more bins of %s have insufficient observations.", var))
  }
  
  df_bins        <- data.table(bins) %>% one_hot()
  names(df_bins) <- paste0(var, "_bin_", c(1:n_bins))
  
  if (intercept) {
    df_bins        <- df_bins[, 2:n_bins]
    names(df_bins) <- paste0(var, "_bin_", c(2:n_bins))
  }
  
  # Return additionally bin centers for the `x_var`
  if (is_xvar) {
    if (bin_type == "uniform") {
      bin_center <- sapply(unique(bins), GetMidpoint) %>% sort()
    } else if (bin_type == "quantile") {
      bin_center <- sapply(unique(bins), GetMeanVal, vals) %>% sort()
    }
    return(list(indicator = df_bins, center = bin_center))
  } else {
    return(df_bins)
  }
}
