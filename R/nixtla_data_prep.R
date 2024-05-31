#' Prepares data for 'TimeGPT''s 'API'
#' This is a private function of 'nixtlar'
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series. Should be named unique_id.
#' @param time_col Column that identifies each timestep. Should be named ds.
#' @param target_col Column that contains the target variable. Should be named y.
#'
#' @return A list with the given or inferred frequency, the prepared data, and the original data frame renamed.
#' @export
#' @keywords internal
#'
#' @examples
#' df <- nixtlar::electricity
#' data <- .nixtla_data_prep(df, freq="H")
#'
.nixtla_data_prep <- function(df, freq, id_col, time_col, target_col){

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
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
  filtered_df <- df[,c("unique_id", "ds", "y")]

  y <- list(
    columns = names(filtered_df),
    data = lapply(1:nrow(filtered_df), function(i) as.list(filtered_df[i,]))
    )

  res <- list(freq = freq,
              y = y
              )

  return(res)
}
