#' Perform cross validation with 'TimeGPT'.
#'
#' @param df A tsibble or a data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param X_df A tsibble or a data frame with future exogenous variables.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param n_windows Number of windows to evaluate.
#' @param step_size Step size between each cross validation window. If NULL, it will equal the forecast horizon (h).
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#'
#' @return A tsibble or a data frame with 'TimeGPT''s cross validation result.
#' @export
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_cross_validation(df, h = 8, id_col = "unique_id", n_windows = 5)
#' }
#'
nixtla_client_cross_validation <- function(df, h=8, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, n_windows=1, step_size=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, model="timegpt-1"){

  # Prepare data ----
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"

  if(is.null(id_col)){
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

  if(is.null(step_size)){
    step_size = h
  }

  timegpt_data <- list(
    model = model,
    fh = h,
    y = y,
    freq = freq,
    n_windows = n_windows,
    step_size = step_size,
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
  url_cv <- "https://dashboard.nixtla.io/api/cross_validation_multi_series"
  resp_cv <- httr2::request(url_cv) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", .get_api_key())
    ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = timegpt_data) |>
    httr2::req_perform()

  # Extract cross-validation ----
  cv <- httr2::resp_body_json(resp_cv)
  cv_list <- lapply(cv$data$forecast$data, unlist)
  res <- data.frame(do.call(rbind, cv_list))
  colnames(res) <- cv$data$forecast$columns
  res[,4:ncol(res)] <- lapply(res[,4:ncol(res)], as.numeric)
  res$cutoff <- lubridate::ymd_hms(res$cutoff)

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
    res$cutoff <- switch(freq,
                         "Y" = as.numeric(substr(res$cutoff, 1, 4)),
                         "A" = as.numeric(substr(res$cutoff, 1, 4)),
                         "Q" = tsibble::yearquarter(res$cutoff),
                         "MS" = tsibble::yearmonth(res$cutoff),
                         "W" = tsibble::yearweek(res$cutoff),
                         "H" = lubridate::ymd_hms(res$cutoff),
                         lubridate::ymd(res$cutoff) # default (daily or other)
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
      res$cutoff <- lubridate::ymd_hms(res$cutoff)
    }else{
      res$ds <- lubridate::ymd(res$ds)
      res$cutoff <- lubridate::ymd(res$cutoff)
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
