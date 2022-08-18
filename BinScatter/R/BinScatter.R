#' Make data for a binned scatter plot and produce it.
#'
#' @param data Data frame for binned scatter plot.
#' @param y_var Name of LHS variable to appear on the y-axis (string).
#' @param x_var Name of RHS variable to appear on the x-axis (string).
#' @param linpartial_var Name(s) of RHS variable(s) to partial `x_var` on linearly (NULL, string, or 
#' list of strings). Note: categorical variables need to be one-hot coded, and are otherwise assumed 
#' continuous.
#' @param binpartial_var Name(s) of RHS variable(s) to partial `x_var` on with bins (NULL, string, or 
#' list of strings).
#' @param nBins Number of bins for `x_var` (integer).
#' @param binType Method to create bins for `x_var` ("uniform" or "quantile"). "uniform" bins are 
#' equally spaced over the support of `x_var`. "quantile" bins have approximately the same number of 
#' observations in each bin.
#' @param nPartialBins Number of bins for variables in `binpartial_var` (integer, default is `nBins`).
#' @param partialBinType Method to create bins for variables in `binpartial_var` ("uniform" or 
#' "quantile", default is `binType`).
#' @param intercept Whether an intercept is included in the regression (boolean).
#' @param ci Whether to include confidence intervals and confidence level (NULL or numeric in (0,1)). 
#' If not NULL, std. err. of the partialed means of `y_var` will be returned in the output dataframe. 
#' If specified but non-numeric or out of range, the default confidence level is 95%.
#' @param dropNA Whether to drop observations with NA in `x_var`, `y_var`, `linpartial_var`, and 
#' `binpartial_var` (boolean). If TRUE, NA will be dropped automatically with a warning message. If 
#' FALSE, returns an error if NA is detected.
#' @param scale_yvar Whether to recenter partialed means of `y_var` (boolean).
#' @param tab_path Path to output the regression table (string).
#' @param plot_path Path to save the binned scatter plot (string).
#' @param plot_xlab X-axis title for the binned scatter plot (string, default it `x_var`).
#' @param plot_ylab Y-axis title for the binned scatter plot (string, default it `y_var`).
#' @param plot_xlim Range of x-axis for the binned scatter plot (Null or c(`min`,`max`), default is 
#' ggplot default).
#' @param plot_ylim Range of y-axis for the binned scatter plot (Null or c(`min`,`max`), default is 
#' ggplot default).
#' @param point_color Color aesthetic to be used with `geom_point()`.
#' @param point_shape Shape aesthetic to be used with `geom_point()`.
#' @param point_fill Fill aesthetic to be used with `geom_point()`.
#' @param point_size Size aesthetic to be used with `geom_point()`.
#' @param bar_width Width aesthetic to be used with `geom_errorbar()`.
#' @param bar_size Size aesthetic to be used with `geom_errorbar()`.
#' @param fig_width Plot width.
#' @param fig_height Plot height.
#' @param axis_title_x_size Text size for x-axis labels.
#' @param axis_title_y_size Text size for y-axis labels.
#' @param axis_text_size Text size for tick labels along axes.
#' @return A list containing the regression summary in the first position and the `ggplot` scatter
#' plot in the second position.
#' 
#' @importFrom dplyr select
#' @importFrom tidyr all_of drop_na
#' @importFrom "data.table" "data.table"
#' @importFrom mltools one_hot bin_data
#' @importFrom purrr reduce
#' @importFrom stats lm nobs qnorm
#' @importFrom stringr str_split str_sub
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 
#' @examples 
#' library(MASS)
#' library(magrittr)
#' library(dplyr)
#' 
#' n <- 10000
#' R <- matrix(c(1, 0.9,
#'               0.9, 1),
#'             nrow = 2, ncol = 2, byrow = TRUE)
#' mu <- c(X = 0, Z = 0)
#' data <- MASS::mvrnorm(n, mu = mu, Sigma = R) %>%
#'   as.data.frame() %>%
#'   mutate(Y_l = X + Z) %>%
#'   mutate(Y_nl = sin(2 * X) + Z)
#'   
#' BinScatter(data = data, x_var = "X", y_var = "Y_nl", binpartial_var = "Z", ci = 0.95)
#' 


BinScatter <- function(data, y_var, x_var, linpartial_var = NULL, binpartial_var = NULL, nBins = 20, 
                       binType = "quantile", nPartialBins = NULL, partialBinType = NULL, 
                       intercept = FALSE, ci = NULL, dropNA = TRUE, scale_yvar = TRUE, 
                       tab_path = NULL, plot_path = NULL, plot_xlab = NULL, plot_ylab = NULL,
                       plot_xlim = NULL, plot_ylim = NULL, point_color = "black", point_shape = 16, 
                       point_fill = "black", point_size = 1, bar_width = 0.1, bar_size = 0.1,
                       fig_width = 7, fig_height = 7, axis_title_x_size = 20, axis_title_y_size = 20, 
                       axis_text_size = 15) {
  
  
  ##------------------------------------------------------------------------------------------------
  ## Define local functions
  ##------------------------------------------------------------------------------------------------

  ## Get midpoint of an interval (bin center for `binType` == "uniform")
  midpoint <- function(interval) {
    
    interval <- as.character(interval) %>% str_sub(2,-2)
    start    <- str_split(interval, ", ")[[1]][1] %>% as.numeric()
    end      <- str_split(interval, ", ")[[1]][2] %>% as.numeric()
    midpoint <- 0.5 * ( start + end )
    
    return(midpoint)
  }
  
  ## Get mean of `x_var` in an interval (bin center for `binType` == "quantile")
  meanval <- function(interval, vals) {
    
    interval <- as.character(interval) %>% str_sub(2,-2)
    start    <- str_split(interval, ", ")[[1]][1] %>% as.numeric()
    end      <- str_split(interval, ", ")[[1]][2] %>% as.numeric()
    meanval  <- vals[start <= vals & vals < end] %>% mean()
    
    return(meanval)
  }
  
  ## Create bin indicators and bin centers for `x_var`
  make_bin_indicator <- function(var, data, nBins, binType, intercept, is_xvar = FALSE) {
    
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
    df_bins        <- data.table(bins) %>% one_hot()
    names(df_bins) <- paste0(var, "_bin_", c(1:nBins))
    
    if (intercept) {
      df_bins        <- df_bins[, 2:nBins]
      names(df_bins) <- paste0(var, "_bin_", c(2:nBins))
    }
    
    # Return additionally bin centers for the `x_var`
    if (is_xvar) {
      if (binType == "uniform") {
        bin_center <- sapply(unique(bins), midpoint) %>% sort()
      } else if (binType == "quantile") {
        bin_center <- sapply(unique(bins), meanval, vals) %>% sort()
      }
      return(list(indicator = df_bins, center = bin_center))
    } else {
      return(df_bins)
    }
  }

  
  ##------------------------------------------------------------------------------------------------
  ## Prepare data for regression
  ##------------------------------------------------------------------------------------------------
  
  ## Clean data
  data <- data %>% as.data.frame()
  data <- data[, all_of(c(y_var, x_var, linpartial_var, binpartial_var))]
  nRaw <- nrow(data)
  data <- data %>% drop_na() 
  nNA  <- nRaw - nrow(data)
  if (nNA > 0) {
    if (dropNA) { 
      warning(sprintf("%s observations dropped due to missing value.", nNA)) 
    } else {
      stop("Missing values present. Set dropNA == TRUE to overide")
    }
  }
  
  df_reg <- data %>% dplyr::select(eval(y_var))
  
  ## Create bin indicators for `x_var`
  xbins  <- make_bin_indicator(x_var, data, nBins, binType, intercept, is_xvar = TRUE)
  x_vals <- xbins[["center"]]
  df_reg <- cbind(df_reg, xbins[["indicator"]])
  
  ## Create bin indicators for variable(s) in `binpartial_var`
  if (!is.null(binpartial_var)) {
    
    if (is.null(nPartialBins))   { nPartialBins <- nBins }
    if (is.null(partialBinType)) { partialBinType <- binType}
    
    df_reg <- cbind(df_reg, lapply(c(binpartial_var), make_bin_indicator, data, nPartialBins, 
                                   partialBinType, intercept) %>% reduce(cbind))
  }
  df_reg <- cbind(df_reg, data[, all_of(c(linpartial_var))])
  df_reg <- df_reg %>% drop_na()
  
  
  ##------------------------------------------------------------------------------------------------
  ## Calculate partialed means (and std. err.) for `y_var` (and recenter)
  ##------------------------------------------------------------------------------------------------
  
  model <- sprintf("%s ~ %s - 1", y_var, paste0(names(df_reg)[-1], collapse = " + "))
  coef <- names(df_reg)[2:(1+nBins)]
  y_mean <- c()
  y_se <- c()
  
  if (intercept) { 
    model <- model %>% str_sub(1,-5)
    coef <- names(df_reg)[2:nBins]
    y_mean <- append(y_mean, 0)
    y_se <- append(y_se, 0)
  }
  reg <- lm(eval(model), df_reg)
  reg_sum <- reg %>% summary()
  y_mean <- append(y_mean, reg$coefficients[coef] %>% unname())
  y_se <- append(y_se, reg_sum$coefficients[coef,"Std. Error"] %>% unname())
  
  ## Write regression table
  if (!is.null(tab_path)) {

    file <- sprintf("%s.txt", tab_path)
    sink(file = file, type = "output")
    print(reg_sum)
    cat("Observations:", nobs(reg))
    sink()
  }

  partialed_mean <- list(mean = y_mean, se = y_se)
  y_vals         <- partialed_mean[["mean"]]
  if (scale_yvar) { y_vals <- y_vals + ( mean( data[,eval(y_var)] ) - mean( y_vals ) ) }
  
  
  ##------------------------------------------------------------------------------------------------
  ## Make and save plot
  ##------------------------------------------------------------------------------------------------
  
  df_plot <- cbind(y_vals, x_vals) %>% as.data.frame()
  if (!is.null(ci)) {
    if (!is.numeric(ci) | ci <= 0 | ci >= 1) {
      ci <- 0.95
      warning("ci is not numeric or outside (0,1), set to 0.95")
    }
    df_plot$y_low <- y_vals + qnorm( (1-ci)/2 ) * partialed_mean[["se"]]
    df_plot$y_up  <- y_vals - qnorm( (1-ci)/2 ) * partialed_mean[["se"]]
  }
 
  if (is.null(plot_xlab)) { plot_xlab <- x_var}
  if (is.null(plot_ylab)) { plot_ylab <- y_var}
  
  plot <- ggplot(df_plot, aes(x = x_vals, y = y_vals)) +
    geom_point(size = point_size, color = point_color, shape = point_shape, fill = point_fill) + 
    labs(x = plot_xlab, y = plot_ylab) +
    theme_bw() +
    {if (!is.null(ci)) 
      geom_errorbar(aes_string(ymin = 'y_low', ymax = 'y_up'), width = bar_width, size=bar_size)} +
    {if (!is.null(plot_xlim)) scale_x_continuous(limits = plot_xlim) } +
    {if (!is.null(plot_ylim)) scale_y_continuous(limits = plot_ylim) } + 
    theme(axis.title.x = element_text(size = axis_title_x_size),
          axis.title.y = element_text(size = axis_title_y_size),
          axis.text = element_text(size = axis_text_size))
  
  if (!is.null(plot_path)) {     
    ggsave(sprintf('%s.pdf', plot_path), width = fig_width, height = fig_height, units = "in")
  }
  
  return(list(reg, plot))
}
