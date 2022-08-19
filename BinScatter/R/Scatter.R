#' Original bin scatter function
#' 
#' @param data Data frame for binned scattr plot
#' @param y_var Name of LHS variable to appear on the y-axis (string)
#' @param x_var Name of RHS variable to appear on the x-axis (string)
#' @param linpartial_var Name(s) of RHS varaible(s) to partial `x_var` on linearly (NULL, string, or 
#' list of strings) Note: categorical variables need to be one-hot coded, and are otherwise assumed 
#' continuous
#' @param binpartial_var Name(s) of RHS varaible(s) to partial `x_var` on with bins (NULL, string, 
#' or list of strings).
#' @param factor_var Name(s) of RHS variables that are factors to partial `x_var` on as indicator 
#' var(s) (NULL, string, or list of strings)
#' @param nBins Number of bins for `x_var` (integer)
#' @param binType Method to create bins for `x_var` ("uniform" or "quantile"). "uniform" bins are 
#' equally spaced over the support of `x_var`. "quantile" bins have approximately the same number of 
#' observations in each bin
#' @param nPartialBins Number of bins for varaibles in `binpartial_var` (integer, defualt is `nBins`)
#' @param partialBinType Method to create bins for varaibles in `binpartial_var` ("uniform" or 
#' "quantile", defualt is `binType`)
#' @param intercept Whether an intercept is included in the regression (boolean)
#' @param ci Whether to include confidence intervals and confidence level (NULL or numeric in (0,1)).
#' If not NULL, std. err. of the partialed means of `y_var` will be retured in the output dataframe. 
#' If specified but out of range, the default confidence level is 95%
#' @param dropNA Whether to drop observations with NA in `x_var`, `y_var`, `linpartial_var`, and 
#' `binpartial_var` (boolean). If TURE, NA will be dropped automatically with a warning message. If 
#' FALSE, returns an error if NA is detected
#' @param scale_yvar Wheather to recenter partialed means of `y_var` (boolean)
#' @param reg_tab Whether to output the regression table in .txt format (boolean)
#' @param tab_name Name of the output regression table (string). Must specify if `reg_tab` is TRUE
#' @param tab_path Path to output the regression table (string). Must specify if `reg_tab` is TRUE
#' @param plot Whether to automatically make and save the binned scatter plot (boolean)
#' @param plot_name Name of the binned scatter plot (string). Must specify if `plot` is TRUE
#' @param plot_path Path to save the binned scatter plot (string). Must specify if `plot` is TRUE
#' @param plot_xlab X-axis title for the binned scatter plot (string, default it `x_var`)
#' @param plot_ylab Y-axis title for the binned scatter plot (string, default it `y_var`)
#' @param plot_xlim Range of x-axis for the binned scatter plot (Null or c(`min`,`max`), default is 
#' ggplot default)
#' @param plot_ylim Range of y-axis for the binned scatter plot (Null or c(`min`,`max`), default is 
#' ggplot default)
#' @param min_obs Minimum number of observations per bin for `x_var`
#' @param partial_min_obs Minimum number of observations per bin for variables in `binpartial_var`
#' @import dplyr
#' @import tidyverse
#' @import ggplot2
#' @import mltools
#' @import "data.table"
#' @export


Scatter <- function(data, y_var, x_var, linpartial_var = NULL, binpartial_var = NULL, factor_var = NULL,
                       nBins = 30, binType = "quantile", nPartialBins = NULL, partialBinType = NULL,
                       intercept = FALSE, ci = NULL, dropNA = TRUE, scale_yvar = TRUE, 
                       reg_tab = TRUE, tab_name = NULL, tab_path = NULL,
                       plot = TRUE, plot_name = NULL, plot_path = NULL, 
                       color = "black", shape = 16, fill = "black",
                       plot_xlab = NULL, plot_ylab = NULL, plot_xlim = NULL, plot_ylim = NULL,
                       axis_title_x_size = 20, axis_title_y_size = 20, axis_text_size = 15, min_obs = 10, partial_min_obs = NULL) {
  
  
  ## Drop NAs in `y_var`, `x_var`, `linpartial_var`, and ` binpartial_var`
  data <- data %>% as.data.frame()
  data <- data[, all_of(c(y_var, x_var, linpartial_var, binpartial_var, factor_var))]
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
  data <- data
  df_reg <- data %>% select(eval(y_var))
  
  ## Create bin indicators and bin centers for `x_var`
  xbins  <- MakeBinIndicator(x_var, data, nBins, binType, intercept, min_obs, is_xvar = TRUE)
  x_vals <- xbins[["center"]]
  df_reg <- xbins[["indicator"]] %>% cbind(df_reg, .)
  
  ## Create bin indicators for variabel(s) in `binpartial_var`
  if (!is.null(binpartial_var)) {
    
    if (is.null(nPartialBins))   { nPartialBins <- nBins }
    if (is.null(partialBinType)) { partialBinType <- binType}
    if (is.null(partial_min_obs)) { partial_min_obs <- min_obs }
    
    df_reg <- lapply(c(binpartial_var), MakeBinIndicator, data, nPartialBins, partialBinType, partial_min_obs, intercept) %>% 
      reduce(cbind) %>%
      cbind(df_reg, .)
  }
  
  ## Calculate partialed means (and std. err.) for `y_var` (and recenter)
  df_reg         <- data[, all_of(c(linpartial_var, factor_var))] %>% cbind(df_reg, .)
  df_reg   <- df_reg %>% drop_na()
  model    <- sprintf("%s ~ %s - 1", y_var, paste0(names(df_reg)[-1], collapse = " + "))
  coef     <- names(df_reg)[2:(1+nBins)]
  y_mean   <- c()
  if (intercept) { 
    model  <- model %>% str_sub(1,-5)
    coef   <- names(df_reg)[2:nBins]
    y_mean <- c(0)
  }
  reg      <- lm(eval(model), df_reg)
  y_mean   <- append(y_mean, reg$coefficients[coef] %>% unname())
  reg_sum  <- reg %>% summary()
  y_se     <- reg_sum$coefficients[coef,"Std. Error"] %>% unname()
  
  # Write regression table
  if (reg_tab) {
    
    if (is.null(tab_name) | is.null(tab_path)) { stop("Name/path of regression table unspecified.") }
    file <- sprintf("%s/%s.txt", tab_path, tab_name)
    sink(file = file, type = "output")
    print(reg_sum)
    cat("Observations:", nobs(reg))
    sink()
  }
  partialed_mean <- list(mean = y_mean, se = y_se)
  y_vals         <- partialed_mean[["mean"]]
  
  if (scale_yvar) { y_vals <- y_vals + ( mean( data[,eval(y_var)] ) - mean( y_vals ) ) }
  
  ## Make output data frame for plot
  df_plot <- cbind(y_vals, x_vals) %>% as.data.frame()
  if (!is.null(ci)) {
    if (!is.numeric(ci) | ci <= 0 | ci >= 1) {
      ci <- 0.95
      warning("ci is not numeric or outside (0,1), set to 0.95")
    }
    df_plot$y_low <- y_vals + qnorm( (1-ci)/2 ) * partialed_mean[["se"]]
    df_plot$y_up  <- y_vals - qnorm( (1-ci)/2 ) * partialed_mean[["se"]]
  }
  
  # Make and save plot
  if (plot) {
    
    if (is.null(plot_name) | is.null(plot_path)) { stop("Name/path of plot unspecified.") }
    if (is.null(plot_xlab)) { plot_xlab <- x_var}
    if (is.null(plot_xlab)) { plot_ylab <- y_var}
    
    plot <- ggplot(df_plot, aes(x = x_vals, y = y_vals)) +
      geom_point(size = 5, color = color, shape = shape, fill = fill) + 
      labs(x = plot_xlab, y = plot_ylab) +
      theme_bw() +
      {if (!is.null(ci)) geom_errorbar(aes(ymin = y_low, ymax = y_up))} +
      {if (!is.null(plot_xlim)) scale_x_continuous(limits = plot_xlim) } +
      {if (!is.null(plot_ylim)) scale_y_continuous(limits = plot_ylim) } + 
      theme(axis.title.x = element_text(size = axis_title_x_size),
            axis.title.y = element_text(size = axis_title_y_size),
            axis.text = element_text(size = axis_text_size))
    
    ggsave(sprintf('%s/%s.pdf', plot_path, plot_name), width = 7.2, height = 7, units = "in")    
  }
  
  return(df_plot)
}
