#' Prepare multiple time series data for TimeGPT
#'
#' @param df A tsibble or a data frame with multiple time series data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#'
#' @return A list with the time series data that will be sent to TimeGPT.
#' @export
#'
prepare_multi_series <- function(df, id_col, time_col, target_col){
  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }
  df <- df[,c(id_col, time_col, target_col)]
  colnames(df) <- c("unique_id", "ds", "y")
  if(tsibble::is_tsibble(df)){
    df$ds <- as.Date(df$ds) # this transforms dates from a tsibble
  }
  y <- list(
    columns = colnames(df),
    data = lapply(1:nrow(df), function(i) as.list(df[i,]))
    )
  return(y)
}
