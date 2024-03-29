#' Get mean of `var` in an interval 
#' 
#' @param interval Interval to get the mean of `var` on (character).
#' @param vals Values of `var`.
#' @importFrom stringr str_split


GetMeanVal <- function(interval, vals) {
  
  interval <- as.character(interval) %>% str_sub(2,-2)
  start    <- str_split(interval, ", ")[[1]][1] %>% as.numeric()
  end      <- str_split(interval, ", ")[[1]][2] %>% as.numeric()
  meanval  <- vals[start <= vals & vals < end] %>% mean()
  
  return(meanval)
}
