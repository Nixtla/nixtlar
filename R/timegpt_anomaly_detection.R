#' Detect anomalies with TimeGPT
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence level (0-100) for the prediction interval used in anomaly detection. Default is 99.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using TimeGPT.
#'
#' @return A tsibble or a data frame with the anomalies detected in the historical period.
#' @export
#'
timegpt_anomaly_detection <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=c(99), clean_ex_first=TRUE){

  # Validation ----
  token <- get("NIXTLAR_TOKEN", envir = nixtlaR_env)

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Prepare data ----
  if(is.null(id_col)){
    url_anomaly <- "Write here the url for the single series case"
  }else{
    url_anomaly <- "https://dashboard.nixtla.io/api/timegpt_multi_series_anomalies"
  }

  data <- timegpt_data(df, freq, id_col, time_col, target_col)
  freq <- data$freq
  y <- data$y

  timegpt_data <- list(
    y = y,
    freq = freq,
    clean_ex_first = clean_ex_first
  )

  # Add exogenous regressors here
  # ----------------------------*

  if(length(level) > 1){
    message("Multiple levels are not allowed for anomaly detection. Will use the largest.")
  }
  level <- as.list(level)
  timegpt_data[["level"]] <- level

  # Make request ----
  resp_anomaly <- httr2::request(url_anomaly) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
    ) |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract anomalies ----
  anomaly <- httr2::resp_body_json(resp_anomaly)
  if(is.null(id_col)){
    # Write here the code for the single series case once the url is available
    res = 42
  }else{
    anomaly_list <- lapply(anomaly$data$forecast$data, unlist)
    res <- data.frame(do.call(rbind, anomaly_list))
    colnames(res) <- anomaly$data$forecast$columns
    res[,3:ncol(res)] <- lapply(res[,3:ncol(res)], as.numeric)
  }

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
  }

  return(res)
}
