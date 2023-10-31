#' Generate TimeGPT forecast for the in-sample period (historical period).
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param finetune_steps Number of steps used to finetune TimeGPT in the new data.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using TimeGPT.
#'
#' @return TimeGPT's forecast for the in-sample period.
#' @export
#'
timegpt_historic <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, finetune_steps=0, clean_ex_first=TRUE){

  # Validation ----
  token <- get("NIXTLAR_TOKEN", envir = nixtlaR_env)

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Prepare data ----
  if(is.null(id_col)){
    url_historic <- "https://dashboard.nixtla.io/api/timegpt_historic"
  }else{
    url_historic <- "https://dashboard.nixtla.io/api/timegpt_multi_series_historic"
  }

  data <- timegpt_data(df, freq, id_col, time_col, target_col)
  freq <- data$freq
  y <- data$y

  timegpt_data <- list(
    y = y,
    freq = freq,
    finetune_steps = finetune_steps,
    clean_ex_first = clean_ex_first
  )

  # Add exogenous regressors here
  # ----------------------------*

  if(!is.null(level)){
    level <- as.list(level)
    timegpt_data[["level"]] <- level
  }

  # Make request ----
  resp_hist <- httr2::request(url_historic) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
    ) |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  hist <- httr2::resp_body_json(resp_hist)

  # Extract fitted values ----
  if(is.null(id_col)){
    idx_fit <- grep("^(timestamp|value|lo|hi)", names(hist$data))
    fit_list <- hist$data[idx_fit]
    fitted <- data.frame(lapply(fit_list, unlist), stringsAsFactors=FALSE)
    colnames(fitted) <- names(fit_list)
    colnames(fitted)[1:2] <- c("ds", "TimeGPT")
    if(!is.null(level)){
      idx_level <- grep("^(lo|hi)", colnames(fitted))
      colnames(fitted)[idx_level] <- paste0("TimeGPT-", colnames(fitted)[idx_level])
    }
  }else{
    fit_list <- lapply(hist$data$forecast$data, unlist)
    fitted <- data.frame(do.call(rbind, fit_list), stringsAsFactors=FALSE)
    colnames(fitted) <- hist$data$forecast$columns
    fitted[,3:ncol(fitted)] <- lapply(fitted[,3:ncol(fitted)], as.numeric)
    fitted <- fitted[,-which(colnames(fitted) == "y")]
  }

  # Convert to tsibble ----
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
  }

  # Rename columns ----
  colnames(fitted)[which(colnames(fitted) == "ds")] <- time_col
  if(!is.null(id_col)){
    colnames(fitted)[which(colnames(fitted) == "unique_id")] <- id_col
  }

  return(fitted)
}
