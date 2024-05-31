#' Detect anomalies with 'TimeGPT'
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence level (0-100) for the prediction interval used in anomaly detection. Default is 99.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#'
#' @return A tsibble or a data frame with the anomalies detected in the historical period.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_anomaly_detection(df, id_col="unique_id")
#' }
#'
nixtla_client_detect_anomalies <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=c(99), clean_ex_first=TRUE, model="timegpt-1"){

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
    model = model,
    y = y,
    freq = freq,
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

  if(length(level) > 1){
    message("Multiple levels are not allowed for anomaly detection. Will use the largest level.")
  }
  level <- as.list(level)
  timegpt_data[["level"]] <- level

  # Make request ----
  url_anomaly <- "https://dashboard.nixtla.io/api/anomaly_detection_multi_series"
  resp_anomaly <- httr2::request(url_anomaly) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", .get_api_key())
    ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract anomalies ----
  anomaly <- httr2::resp_body_json(resp_anomaly)
  anomaly_list <- lapply(anomaly$data$forecast$data, unlist)
  res <- data.frame(do.call(rbind, anomaly_list))
  colnames(res) <- anomaly$data$forecast$columns
  res[,3:ncol(res)] <- lapply(res[,3:ncol(res)], as.numeric)

  # Data transformation ----
  if(tsibble::is_tsibble(df)){
    res$ds <- switch(freq,
                     "Y" = as.numeric(substr(res$ds, 1, 4)),
                     "A" = as.numeric(substr(res$ds, 1, 4)),
                     "Q" = tsibble::yearquarter(res$ds),
                     "MS" = tsibble::yearmonth(res$ds),
                     "W" = tsibble::yearweek(res$ds),
                     "H" = lubridate::ymd_hms(res$ds),
                     lubridate::ymd(res$ds) # default (daily or other)
    )
    if(is.null(id_col)){
      res <- tsibble::as_tsibble(res, index="ds")
    }else{
      res <- tsibble::as_tsibble(res, key="unique_id", index="ds")
    }
  }else{
    # If df is a data frame, convert ds to dates
    if(freq == "H"){
      res$ds <- lubridate::ymd_hms(res$ds)
    }else{
      res$ds <- lubridate::ymd(res$ds)
    }
  }

  # Rename columns ----
  colnames(res)[which(colnames(res) == "ds")] <- time_col
  if(!is.null(id_col)){
    colnames(res)[which(colnames(res) == "unique_id")] <- id_col
  }else{
    # remove unique_id column
    res <- res |>
      dplyr::select(-c(.data$unique_id))
  }

  return(res)
}
