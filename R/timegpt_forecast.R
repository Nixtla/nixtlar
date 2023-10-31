#' Generate TimeGPT forecast
#'
#' @param df A tsibble or a data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param finetune_steps Number of steps used to finetune TimeGPT in the new data.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using TimeGPT.
#' @param add_history Return fitted values of the model.
#'
#' @return TimeGPT forecasts for point predictions and probabilistic predictions (if level is not NULL).
#' @export
#'
timegpt_forecast <- function(df, h=8, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, finetune_steps=0, clean_ex_first=TRUE, add_history=FALSE){

  token <- get("NIXTLAR_TOKEN", envir = nixtlaR_env)

  if(!tsibble::is_tsibble(df) & !is.data.frame(df)){
    stop("Only tsibbles or data frames are allowed.")
  }

  # Rename columns
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"
  if(!is.null(id_col)){
    names(df)[which(names(df) == id_col)] <- "unique_id"
  }

  # If df is a tsibble, convert dates to strings and infer frequency
  if(tsibble::is_tsibble(df)){
    res <- date_conversion(df)
    df <- res$df
    freq <- res$freq
  }

  # Infer frequency if not available
  if(is.null(freq)){
    freq <- infer_frequency(df)
  }

  # Check if single or multi-series and prepare data
  if(is.null(id_col)){
    url <- "https://dashboard.nixtla.io/api/timegpt"
    series_type <- "single"
  }else{
    url <- "https://dashboard.nixtla.io/api/timegpt_multi_series"
    series_type <- "multi"
  }

  y <- prepare_data(df)

  timegpt_data <- list(
    fh = h,
    y = y,
    freq = freq,
    finetune_steps = finetune_steps,
    clean_ex_first = clean_ex_first
    )

  # if(!is.null(X_df)){
  #   names(X_df)[which(names(X_df) == time_col)] <- "ds"
  #   if(!is.null(id_col)){
  #     names(X_df)[which(names(X_df) == id_col)] <- "unique_id"
  #   }
  #   x <- list(
  #     columns = names(X_df),
  #     data = lapply(1:nrow(X_df), function(i) as.list(X_df[i,]))
  #   )
  #   timegpt_data[["x"]] <- x
  # }

  if(!is.null(level)){
    level <- as.list(level) # TimeGPT requires level to be a list.
    # Users of the forecast package are used to define the level as a vector.
    timegpt_data[["level"]] <- level
  }

  # Make request
  resp <- httr2::request(url) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
      ) |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract TimeGPT forecast
  fc <- httr2::resp_body_json(resp)
  if(series_type == "single"){
    idx <- grep("^(timestamp|value|lo|hi)", names(fc$data))
    fc_list <- fc$data[idx]
    fcst <- data.frame(lapply(fc_list, unlist), stringsAsFactors=FALSE)
    colnames(fcst) <- names(fc_list)
    colnames(fcst)[1:2] <- c("ds", "TimeGPT")
    if(!is.null(level)){
      idx_level <- grep("^(lo|hi)", colnames(fcst))
      colnames(fcst)[idx_level] <- paste0("TimeGPT-", colnames(fcst)[idx_level])
    }

  }else{
    fc_list <- lapply(fc$data$forecast$data, unlist)
    fcst <- data.frame(do.call(rbind, fc_list))
    colnames(fcst) <- fc$data$forecast$columns
    fcst[,3:ncol(fcst)] <- lapply(fcst[,3:ncol(fcst)], as.numeric)
  }

  if(add_history){
    if(series_type == "single"){
      url_historic <- "https://dashboard.nixtla.io/api/timegpt_historic"
      timegpt_historic <- list(
        y = y,
        freq = freq,
        clean_ex_first = clean_ex_first
      )

      if(!is.null(level)){
        timegpt_historic[["level"]] <- level
      }

      # Make request
      resp_hist <- httr2::request(url_historic) |>
        httr2::req_headers(
          "accept" = "application/json",
          "content-type" = "application/json",
          "authorization" = paste("Bearer", token)
        ) |>
        httr2::req_body_json(data = timegpt_historic) |>
        httr2::req_perform()

      hist <- httr2::resp_body_json(resp_hist)

      # Extract fitted values
      idx_fit <- grep("^(timestamp|value|lo|hi)", names(hist$data))
      fit_list <- hist$data[idx_fit]
      fitted <- data.frame(lapply(fit_list, unlist), stringsAsFactors=FALSE)
      colnames(fitted) <- names(fit_list)
      colnames(fitted)[1:2] <- c("ds", "TimeGPT")
      if(!is.null(level)){
        idx_level <- grep("^(lo|hi)", colnames(fitted))
        colnames(fitted)[idx_level] <- paste0("TimeGPT-", colnames(fitted)[idx_level])
      }

      # Combine fitted values and TimeGPT forecast
      fcst <- rbind(fitted, fcst)

    }else{
      #series_type == "multi"
      url_historic <- "https://dashboard.nixtla.io/api/timegpt_multi_series_historic"
      timegpt_historic <- list(
        y = y,
        freq = freq,
        clean_ex_first = clean_ex_first
      )

      if(!is.null(level)){
        timegpt_historic[["level"]] <- level
      }

      # Make request
      resp_hist <- httr2::request(url_historic) |>
        httr2::req_headers(
          "accept" = "application/json",
          "content-type" = "application/json",
          "authorization" = paste("Bearer", token)
        ) |>
        httr2::req_body_json(data = timegpt_historic) |>
        httr2::req_perform()

      hist <- httr2::resp_body_json(resp_hist)

      # Extract fitted values
      fit_list <- lapply(hist$data$forecast$data, unlist)
      fitted <- data.frame(do.call(rbind, fit_list), stringsAsFactors=FALSE)
      colnames(fitted) <- hist$data$forecast$columns
      fitted[,3:ncol(fitted)] <- lapply(fitted[,3:ncol(fitted)], as.numeric)
      fitted <- fitted[,-which(colnames(fitted) == "y")]

      fcst <- rbind(fitted, fcst)
    }
  }

  # Return a tsibble if the input was a tsibble
  if(tsibble::is_tsibble(df)){
    if(freq == "H"){
      fcst$ds <- lubridate::ymd_hms(fcst$ds)
    }else{
      fcst$ds <- lubridate::ymd(fcst$ds)
    }
    fcst <- tsibble::as_tsibble(fcst, key="unique_id", index="ds")
  }

  # Rename columns to original names
  colnames(fcst)[which(colnames(fcst) == "ds")] <- time_col
  if(!is.null(id_col)){
    colnames(fcst)[which(colnames(fcst) == "unique_id")] <- id_col
  }

  return(fcst)
}
