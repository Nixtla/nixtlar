#' Generate 'TimeGPT' forecast
#'
#' @param df A tsibble or a data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param add_history Return fitted values of the model.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#'
#' @return 'TimeGPT''s forecast.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_forecast(df, h=8, id_col="unique_id", level=c(80,95))
#' }
#'
nixtla_client_forecast <- function(df, h=8, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, add_history=FALSE, model="timegpt-1"){

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
    fh = h,
    y = y,
    freq = freq,
    finetune_steps = finetune_steps,
    finetune_loss = finetune_loss,
    clean_ex_first = clean_ex_first
    )

  if(!is.null(X_df)){
    names(X_df)[which(names(X_df) == time_col)] <- "ds"
    if(is.null(id_col)){
      X_df <- X_df |>
        dplyr::mutate(unique_id = "ts_0") |>
        dplyr::select(c("unique_id", tidyselect::everything()))
    }else{
      names(X_df)[which(names(X_df) == id_col)] <- "unique_id"
    }

    # Validation checks for exogenous variables
    status <- .validate_exogenous(df, h, X_df)
    if(!status$validation){
      stop(status$message)
    }

    exogenous <-  df |>
      dplyr::select(-y)

    exogenous <- rbind(exogenous, X_df)

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
  url <- "https://dashboard.nixtla.io/api/forecast_multi_series"
  resp <- httr2::request(url) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", .get_api_key())
      ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract forecast ----
  fc <- httr2::resp_body_json(resp)
  fc_list <- lapply(fc$data$forecast$data, unlist)
  fcst <- data.frame(do.call(rbind, fc_list))
  names(fcst) <- fc$data$forecast$columns
  if(!is.null(level)){
    fcst[,3:ncol(fcst)] <- lapply(fcst[,3:ncol(fcst)], as.numeric)
  }else{
    fcst$TimeGPT <- as.numeric(fcst$TimeGPT)
  }

  # Data transformation ----
  if(tsibble::is_tsibble(df)){
    fcst$ds <- switch(freq,
                      "Y" = as.numeric(substr(fcst$ds, 1, 4)),
                      "A" = as.numeric(substr(fcst$ds, 1, 4)),
                      "Q" = tsibble::yearquarter(fcst$ds),
                      "MS" = tsibble::yearmonth(fcst$ds),
                      "W" = tsibble::yearweek(fcst$ds),
                      "H" = lubridate::ymd_hms(fcst$ds),
                      lubridate::ymd(fcst$ds) # default (daily or other)
                      )
    if(is.null(id_col)){
      fcst <- tsibble::as_tsibble(fcst, index="ds")
    }else{
      fcst <- tsibble::as_tsibble(fcst, key="unique_id", index="ds")
    }
  }else{
    if(freq == "H"){
      fcst$ds <- lubridate::ymd_hms(fcst$ds)
    }else{
      fcst$ds <- lubridate::ymd(fcst$ds)
    }
  }

  # Rename columns ----
  names(fcst)[which(names(fcst) == "ds")] <- time_col
  if(!is.null(id_col)){
    names(fcst)[which(names(fcst) == "unique_id")] <- id_col
  }else{
    # remove unique_id column
    fcst <- fcst |>
      dplyr::select(-c(.data$unique_id))
  }

  # Generate fitted values ----
  if(add_history){
    fitted <- nixtla_client_historic(df, freq=freq, id_col=id_col, time_col=time_col, target_col=target_col, level=level, finetune_steps=finetune_steps, clean_ex_first=clean_ex_first)
    if(tsibble::is_tsibble(df)){
      fcst <- dplyr::bind_rows(fitted, fcst)
    }else{
      fcst <- rbind(fitted, fcst)
    }
  }

  return(fcst)
}
