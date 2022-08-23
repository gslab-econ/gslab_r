#' Make data for a binned scatter plot and produce it.
#'
#' @param data Data frame for binned scatter plot.
#' @param y_var Name of LHS variable to appear on the y-axis (character).
#' @param x_var Name of RHS variable to appear on the x-axis (character).
#' @param linpartial_var Names of RHS variables to partial `x_var` on linearly (character). 
#' * Note: categorical variables need to be one-hot coded, and are otherwise assumed continuous.
#' @param binpartial_var Names of RHS variables to partial `x_var` on with bins (character).
#' @param n_bins Number of bins for `x_var` (integer).
#' @param bin_type Method to create bins for `x_var` ("uniform" or "quantile")
#' * "uniform" bins are equally spaced over the support of `x_var`. 
#' * "quantile" bins have approximately the same number of observations in each bin.
#' @param n_partial_bins Number of bins for variables in `binpartial_var` (integer, default is 
#' `n_bins`).
#' @param partial_bin_type Method to create bins for variables in `binpartial_var` ("uniform" or 
#' "quantile", default is `bin_type`).
#' @param min_obs Minimum number of observations per bin for `x_var` (integer, default is 10).
#' @param min_partial_obs Minimum number of observations per bin for variables in `binpartial_var`
#' (integer, default is `min_obs`).
#' @param intercept Whether an intercept is included in the regression (boolean).
#' @param ci Whether to include confidence intervals (numeric in (0,1) specifying confidence level). 
#' * If not `NULL`, standard errors of the partialed means of `y_var` will be returned in the output 
#' dataframe. 
#' * If specified but non-numeric or out of range, the default confidence level is 0.95.
#' @param drop_na Whether to drop observations with `NA` in `x_var`, `y_var`, `linpartial_var`, and 
#' `binpartial_var` (boolean). 
#' * If `TRUE`, `NA` will be dropped automatically with a warning message. 
#' * If `FALSE`, returns an error if `NA` is detected.
#' @param recenter Whether to recenter partialed means of `y_var` (boolean).
#' @param tab_path Whether to save the regression table output (character).
#' * If not `NULL`, saves output to text file at `tab_path`.
#' @param plot_path Whether to save the binned scatter plot (character).
#' * If not `NULL`, saves plot to PDF file at `plot_path`.
#' @param plot_xlab X-axis title for the binned scatter plot (character, default is `x_var`).
#' @param plot_ylab Y-axis title for the binned scatter plot (character, default is `y_var`).
#' @param plot_xlim Range of x-axis for the binned scatter plot (`c(min, max)`, default is 
#' ggplot default).
#' @param plot_ylim Range of y-axis for the binned scatter plot (`c(min, max)`, default is 
#' ggplot default).
#' @param point_color Color aesthetic to be used with `geom_point()`.
#' @param point_fill Fill aesthetic to be used with `geom_point()`.
#' @param point_shape Shape aesthetic to be used with `geom_point()`.
#' @param point_size Size aesthetic to be used with `geom_point()`.
#' @param bar_width Width aesthetic to be used with `geom_errorbar()`.
#' @param bar_size Size aesthetic to be used with `geom_errorbar()`.
#' @param axis_title_x_size Text size for x-axis labels.
#' @param axis_title_y_size Text size for y-axis labels.
#' @param axis_text_size Text size for tick labels along axes.
#' @param fig_width Plot width in inches.
#' @param fig_height Plot height in inches.
#' 
#' @importFrom dplyr select
#' @importFrom tidyr all_of drop_na
#' @importFrom purrr reduce
#' @importFrom stats lm nobs qnorm
#' @importFrom stringr str_sub
#' @importFrom ggplot2 ggplot ggsave aes geom_point geom_errorbar labs theme_bw theme element_text
#' scale_x_continuous scale_y_continuous 
#' @importFrom magrittr "%>%"
#' @export
#' 

BinScatter <- function(data, y_var, x_var, linpartial_var = NULL, binpartial_var = NULL, n_bins = 20,
                    bin_type = "quantile", n_partial_bins = NULL, partial_bin_type = NULL,
                    min_obs = 10, min_partial_obs = NULL, intercept = FALSE, ci = NULL,
                    drop_na = TRUE, recenter = TRUE, tab_path = NULL, plot_path = NULL,
                    plot_xlab = NULL, plot_ylab = NULL, plot_xlim = NULL, plot_ylim = NULL,
                    point_color = "black", point_fill = NA, point_shape = 19, point_size = 1.5,
                    bar_width = 0.1, bar_size = 0.5, axis_title_x_size = NULL, 
                    axis_title_y_size = NULL, axis_text_size = NULL, fig_width = 7, fig_height = 7) {
  
  ## Drop NAs in `y_var`, `x_var`, `linpartial_var`, and ` binpartial_var`
  data <- data %>% as.data.frame()
  data <- data[, all_of(c(y_var, x_var, linpartial_var, binpartial_var))]
  nRaw <- nrow(data)
  data <- data %>% drop_na() 
  nNA  <- nRaw - nrow(data)
  
  if (nNA > 0) {
    if (drop_na) { 
      warning(sprintf("%s observations dropped due to missing value.", nNA)) 
    } else {
      stop("Missing values present. Set drop_na == TRUE to overide.")
    }
  }
  data   <- data
  df_reg <- data %>% dplyr::select(eval(y_var))
  
  ## Create bin indicators and bin centers for `x_var`
  xbins  <- MakeBinIndicator(x_var, data, n_bins, bin_type, min_obs, intercept, is_xvar = TRUE)
  x_vals <- xbins[["center"]]
  df_reg <- xbins[["indicator"]] %>% cbind(df_reg, .)
  
  ## Create bin indicators for variable(s) in `binpartial_var`
  if (!is.null(binpartial_var)) {
    
    if (is.null(n_partial_bins))   { n_partial_bins <- n_bins }
    if (is.null(partial_bin_type)) { partial_bin_type <- bin_type }
    if (is.null(min_partial_obs))  { min_partial_obs <- min_obs }
    
    df_reg <- lapply(c(binpartial_var), MakeBinIndicator, data, n_partial_bins, partial_bin_type, 
                     min_partial_obs, intercept) %>% reduce(cbind) %>% cbind(df_reg, .)
  }
  
  ## Calculate partialed means for `y_var`
  df_reg <- data[, all_of(c(linpartial_var))] %>% cbind(df_reg, .)
  df_reg <- df_reg %>% drop_na()
  model  <- sprintf("%s ~ %s - 1", y_var, paste0(names(df_reg)[-1], collapse = " + "))
  coef   <- names(df_reg)[2:(1+n_bins)]
  
  if (intercept) { 
    model  <- model %>% str_sub(1,-5)
    coef   <- names(df_reg)[2:n_bins]
    y_mean <- c(0)
    y_se   <- c(0)
  } else {
    y_mean <- c()
    y_se   <- c()
  }
  
  reg      <- lm(eval(model), df_reg)
  y_mean   <- append(y_mean, reg$coefficients[coef] %>% unname())
  reg_sum  <- reg %>% summary()
  y_se     <- append(y_se, reg_sum$coefficients[coef,"Std. Error"] %>% unname())
  
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
  
  if (recenter) { y_vals <- y_vals + ( mean( data[,eval(y_var)] ) - mean( y_vals ) ) }
  
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
  
  ## Make and save plot
  if (!is.null(plot_path)) {
    
    if (is.null(plot_xlab)) { plot_xlab <- x_var }
    if (is.null(plot_ylab)) { plot_ylab <- y_var }
    
    plot <- ggplot(df_plot, aes(x = x_vals, y = y_vals)) +
      {if (!is.null(ci)) 
        geom_errorbar(aes(ymin = y_low, ymax = y_up), width = bar_width, size = bar_size) } +
      geom_point(size = point_size, color = point_color, shape = point_shape, fill = point_fill) + 
      labs(x = plot_xlab, y = plot_ylab) +
      {if (!is.null(plot_xlim)) scale_x_continuous(limits = plot_xlim) } +
      {if (!is.null(plot_ylim)) scale_y_continuous(limits = plot_ylim) } + 
      theme_bw() +
      theme(axis.title.x = element_text(size = axis_title_x_size),
            axis.title.y = element_text(size = axis_title_y_size),
            axis.text = element_text(size = axis_text_size))
    
    ggsave(sprintf('%s.pdf', plot_path), width = fig_width, height = fig_height, units = "in")    
  }
  
  return(df_plot)
}
