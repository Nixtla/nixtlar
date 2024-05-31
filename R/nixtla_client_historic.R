#' Generate 'TimeGPT' forecast for the in-sample period (historical period).
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#'
#' @return 'TimeGPT''s forecast for the in-sample period.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_historic(df, id_col="unique_id", level=c(80,95))
#' }
#'
nixtla_client_historic <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE){

  # Prepare data ----
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"

  if(is.null(id_col)){
    # create unique_id for single series
    df <- df |>
      dplyr::mutate(unique_id = "ts_0") |>
      dplyr::select(c("unique_id", tidyselect::everything()))
  }else{
    # id_col is not NULL
    names(df)[which(names(df) == id_col)] <- "unique_id"
  }

  data <- .nixtla_data_prep(df, freq, id_col, time_col, target_col)
  freq <- data$freq
  y <- data$y

  timegpt_data <- list(
    y = y,
    freq = freq,
    finetune_steps = finetune_steps,
    finetune_loss = finetune_loss,
    clean_ex_first = clean_ex_first
  )

  if(!any(names(df) %in% c("unique_id", "ds", "y"))){
    # input includes exogenous variables
    exogenous <-  df |>
      dplyr::select(-c(.data$y))

    x <- list(
      columns = names(exogenous),
      data = lapply(1:nrow(exogenous), function(i) as.list(exogenous[i,]))
    )

    timegpt_data[['x']] <- x
  }

  if(!is.null(level)){
    level <- as.list(level)
    timegpt_data[["level"]] <- level
  }

  # Make request ----
  url_historic <- "https://dashboard.nixtla.io/api/historic_forecast_multi_series"
  resp_hist <- httr2::request(url_historic) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", .get_api_key())
    ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract fitted values ----
  hist <- httr2::resp_body_json(resp_hist)
  fit_list <- lapply(hist$data$forecast$data, unlist)
  fitted <- data.frame(do.call(rbind, fit_list), stringsAsFactors=FALSE)
  names(fitted) <- hist$data$forecast$columns
  fitted[,3:ncol(fitted)] <- lapply(fitted[,3:ncol(fitted)], as.numeric)
  fitted <- fitted[,-which(names(fitted) == "y")]

  # Data transformation ----
  if(tsibble::is_tsibble(df)){
    fitted$ds <- switch(freq,
                        "Y" = as.numeric(substr(fitted$ds, 1, 4)),
                        "A" = as.numeric(substr(fitted$ds, 1, 4)),
                        "Q" = tsibble::yearquarter(fitted$ds),
                        "MS" = tsibble::yearmonth(fitted$ds),
                        "W" = tsibble::yearweek(fitted$ds),
                        "H" = lubridate::ymd_hms(fitted$ds),
                        lubridate::ymd(fitted$ds)) # default (daily "D" or other)
    if(is.null(id_col)){
      fitted <- tsibble::as_tsibble(fitted, index="ds")
    }else{
      fitted <- tsibble::as_tsibble(fitted, key="unique_id", index="ds")
    }
  }else{
    # If df is a data frame, convert ds to dates
    if(freq == "H"){
      fitted$ds <- lubridate::ymd_hms(fitted$ds)
    }else{
      fitted$ds <- lubridate::ymd(fitted$ds)
    }
  }

  # Rename columns ----
  names(fitted)[which(names(fitted) == "ds")] <- time_col
  if(!is.null(id_col)){
    names(fitted)[which(names(fitted) == "unique_id")] <- id_col
  }else{
    # remove unique_id column
    fitted <- fitted |>
      dplyr::select(-c(.data$unique_id))
  }

  return(fitted)
}
