#' Prepare time series data for TimeGPT's API
#'
#' @param df A tsibble or a data frame with time series data.
#'
#' @return A list with the time series data for TimeGPT's API.
#' @export
#'
prepare_data <- function(df){
  if("unique_id" %in% names(df)){
    df <- df[, c("unique_id", "ds", "y")]
    y <- list(
      columns = names(df),
      data = lapply(1:nrow(df), function(i) as.list(df[i,]))
      )
  }else{
      # only "ds" and "y" columns
      y <- df$y
      names(y) <- df$ds
      y <- as.list(y)
  }
  return(y)
}
