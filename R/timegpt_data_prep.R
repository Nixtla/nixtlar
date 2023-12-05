#' Prepares data for TimeGPT's API
#' This is a private function of the package
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#'
#' @return A list with the given or inferred frequency, the prepared data, and the original data frame renamed.
#'
.timegpt_data_prep <- function(df, freq, id_col, time_col, target_col){

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Rename columns
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"
  if(!is.null(id_col)){
    names(df)[which(names(df) == id_col)] <- "unique_id"
  }

  # If df is a tsibble, convert dates to strings and infer frequency if necessary
  if(tsibble::is_tsibble(df)){
    res <- date_conversion(df)
    df <- res$df
    freq <- res$freq
  }

  # Infer frequency if not available
  if(is.null(freq)){
    freq <- infer_frequency(df)
  }

  # Prepare data
  if("unique_id" %in% names(df)){
    df <- df[,c("unique_id", "ds", "y")]
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

  res <- list(freq = freq,
              y = y
              )

  return(res)
}
