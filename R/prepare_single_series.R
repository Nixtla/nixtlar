#' Prepare single time series data for TimeGPT
#'
#' @param df A tsibble or a data frame with a single time series data.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#'
#' @return A list with the time series data that will be sent to TimeGPT.
#' @export
#'
prepare_single_series <- function(df, time_col, target_col){

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  df <- df[,c(time_col, target_col)]
  colnames(df) <- c("ds", "y")
  if(tsibble::is_tsibble(df)){
    df$ds <- as.Date(df$ds) # this transforms dates from a tsibble
  }
  y <- df$y
  names(y) <- df$ds
  y <- as.list(y)
  return(y)
}
