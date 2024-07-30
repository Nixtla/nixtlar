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

  # Rename columns and add unique identifier if necessary
  # Transform time column if applicable (for example, if it is in yearmonth format)
  # Infer frequency if not supplied by the user
  # Extract timezone if available

  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"

  if(is.null(id_col)){
    # add series id
    if(tsibble::is_tsibble(df)){
      df <- df |>
        dplyr::mutate(unique_id = "ts_0") |>
        dplyr::select(c("unique_id", tidyselect::everything())) |>
        tsibble::as_tsibble(index = ds, key = unique_id)
    }else{
      df <- df |>
        dplyr::mutate(unique_id = "ts_0") |>
        dplyr::select(c("unique_id", tidyselect::everything()))
    }
  }else{
    names(df)[which(names(df) == id_col)] <- "unique_id"
  }

  cls <- class(df$ds)[1]
  if(cls != "character"){
    res <- .date_conversion(df, freq, cls)
    df <- res$df
    freq <- res$freq
    tzone <- res$tzone
  }

  # Infer frequency if still not available
  if(is.null(freq)){
    freq <- infer_frequency(df)
  }

  # Prepare data
  # filtered_df <- df[,c("unique_id", "ds", "y")]
  #
  # y <- list(
  #   columns = names(filtered_df),
  #   data = lapply(1:nrow(filtered_df), function(i) as.list(filtered_df[i,]))
  #   )
  #
  # res <- list(freq = freq,
  #             y = y
  #             )

  return(res)
}
