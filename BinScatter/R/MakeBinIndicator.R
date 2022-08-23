#' Create bin indicators and bin centers
#' 
#' @param var Name of variable to create bins for (character).
#' @param data Data frame with column `var`.
#' @param n_bins Number of bins for `var` (integer).
#' @param bin_type Method to create bins for `var` ("uniform" or "quantile")
#' * "uniform" bins are equally spaced over the support of `var`. 
#' * "quantile" bins have approximately the same number of observations in each bin.
#' @param intercept Whether an intercept is included in the regression (boolean).
#' * If `TRUE`, omits the indicator for the first bin of `var`.
#' @param min_obs Minimum number of observations per bin for `var` (integer).
#' @param is_xvar Whether `var` is x-axis variable (boolean).
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
  
  ## Create a data frame of bin indicators
  vals <- data[, eval(var)] %>% as.numeric()
  bins <- bin_data(vals, bins = n_bins, binType = temp_bin_type) %>% factor(ordered = FALSE)
  if (n_bins != length(table(bins)[table(bins) >= min_obs])) {
    stop(sprintf("One or more bins of %s have insufficient observations.", var))
  }
  
  df_bins <- data.table(bins) %>% one_hot()
  names(df_bins) <- paste0(var, "_bin_", c(1:n_bins))
  
  if (intercept) {
    df_bins <- df_bins[, 2:n_bins]
    names(df_bins) <- paste0(var, "_bin_", c(2:n_bins))
  }
  
  ## Return additionally bin centers for the `x_var`
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
