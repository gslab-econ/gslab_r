#' Get midpoint of an interval
#' 
#' @param interval Interval to get the midpoint of (character).
#' @importFrom stringr str_split str_sub


GetMidpoint <- function(interval) {
  
  interval <- as.character(interval) %>% str_sub(2,-2)
  start    <- str_split(interval, ", ")[[1]][1] %>% as.numeric()
  end      <- str_split(interval, ", ")[[1]][2] %>% as.numeric()
  midpoint <- 0.5 * ( start + end )
  
  return(midpoint)
}
