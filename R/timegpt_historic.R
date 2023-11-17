#' Generate TimeGPT forecast for the in-sample period (historical period).
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param finetune_steps Number of steps used to finetune TimeGPT in the new data.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using TimeGPT.
#'
#' @return TimeGPT's forecast for the in-sample period.
#' @export
#'
timegpt_historic <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=NULL, finetune_steps=0, clean_ex_first=TRUE){

  # Validation ----
  token <- .get_token()

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Prepare data ----
  if(is.null(id_col)){
    url_historic <- "https://dashboard.nixtla.io/api/timegpt_historic"
  }else{
    url_historic <- "https://dashboard.nixtla.io/api/timegpt_multi_series_historic"
  }

  data <- .timegpt_data_prep(df, freq, id_col, time_col, target_col)
  df <- data$df
  freq <- data$freq
  y <- data$y

  timegpt_data <- list(
    y = y,
    freq = freq,
    finetune_steps = finetune_steps,
    clean_ex_first = clean_ex_first
  )

  if(any(!(names(df) %in% c("unique_id", "ds", "y")))){
    exogenous <- df |>
      dplyr::select(-y)

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
  resp_hist <- httr2::request(url_historic) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
    ) |>
    httr2::req_user_agent("nixtla-r") |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract fitted values ----
  hist <- httr2::resp_body_json(resp_hist)
  if(is.null(id_col)){
    idx_fit <- grep("^(timestamp|value|lo|hi)", names(hist$data))
    fit_list <- hist$data[idx_fit]
    fitted <- data.frame(lapply(fit_list, unlist), stringsAsFactors=FALSE)
    names(fitted) <- names(fit_list)
    names(fitted)[1:2] <- c("ds", "TimeGPT")
    if(!is.null(level)){
      idx_level <- grep("^(lo|hi)", names(fitted))
      names(fitted)[idx_level] <- paste0("TimeGPT-", names(fitted)[idx_level])
    }
  }else{
    fit_list <- lapply(hist$data$forecast$data, unlist)
    fitted <- data.frame(do.call(rbind, fit_list), stringsAsFactors=FALSE)
    names(fitted) <- hist$data$forecast$columns
    fitted[,3:ncol(fitted)] <- lapply(fitted[,3:ncol(fitted)], as.numeric)
    fitted <- fitted[,-which(names(fitted) == "y")]
  }

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
  }

  return(fitted)
}
