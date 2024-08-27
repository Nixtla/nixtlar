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
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param add_history Return fitted values of the model.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
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
nixtla_client_forecast <- function(df, h=8, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, quantiles=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, add_history=FALSE, model="timegpt-1", num_partitions=NULL){

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

  if(!is.null(level) & !is.null(quantiles)){
    stop("You should include 'level' or 'quantiles' but not both.")
  }else if(!is.null(level)){
    if(any(level < 0 | level > 100)){
      stop("Level should be between 0 and 100")
    }
    level <- as.list(level)
    timegpt_data[["level"]] <- level
  }else if(!is.null(quantiles)){
    if(any(quantiles < 0 | quantiles > 1)){
      stop("Quantiles should be between 0 and 1.")
    }
    lvl <- .level_from_quantiles(quantiles)
    level <- as.list(lvl$level)
    timegpt_data[["level"]] <- level
  }

  # Create request ----
  url <- "https://api.nixtla.io/forecast_multi_series"

  payload_list <- .partition_payload(timegpt_data, num_partitions)

  future::plan(future::multisession)

  responses <- .make_request(url, payload_list)

  fcst_list <- lapply(responses, function(resp) {
    fc_list <- lapply(resp$data$forecast$data, unlist)
    fc <- data.frame(do.call(rbind, fc_list))
    names(fc) <- resp$data$forecast$columns
    return(fc)
  })

  fcst <- do.call(rbind, fcst_list)
  if(!is.null(level)){
    fcst[, 3:ncol(fcst)] <- future.apply::future_lapply(fcst[, 3:ncol(fcst)], as.numeric)
  }else{
    fcst$TimeGPT <- do.call(c, future.apply::future_lapply(fcst$TimeGPT, as.numeric))
  }

  # Rename quantile columns if necessary
  if(!is.null(quantiles)){
    cols_table <- lvl$ql_df$quantiles_col
    names(cols_table) <- lvl$ql_df$level_col
    names(fcst) <- ifelse(names(fcst) %in% names(cols_table), cols_table[names(fcst)], names(fcst))

    fcst <- fcst[, !grepl("hi|lo", names(fcst))]

    # Add 0.5 quantile if present
    if(0.5 %in% quantiles){
      fcst <- fcst |>
        mutate(quantile50 = .data$TimeGPT)
      names(fcst)[which(names(fcst) == "quantile50")] <- "TimeGPT-q-50"
    }

    if(is.null(id_col)){
      fcst <- fcst |>
        dplyr::select(.data$ds, .data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
        dplyr::select(.data$ds, .data$TimeGPT, sort(tidyselect::peek_vars()))
    }else{
      fcst <- fcst |>
        dplyr::select(.data$unique_id, .data$ds, .data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
        dplyr::select(.data$unique_id, .data$ds, .data$TimeGPT, sort(tidyselect::peek_vars()))
    }
  }

  # Date transformation ----
  fcst <- .transform_output_dates(fcst, id_col, "ds", freq, data$flag)

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
    if(!is.null(quantiles)){
      level <- NULL # delete previously generated level
    }
    fitted <- nixtla_client_historic(df, freq=freq, id_col=id_col, time_col=time_col, target_col=target_col, level=level, quantiles=quantiles, finetune_steps=finetune_steps, clean_ex_first=clean_ex_first)
    if(tsibble::is_tsibble(df)){
      fcst <- dplyr::bind_rows(fitted, fcst)
    }else{
      fcst <- rbind(fitted, fcst)
    }
  }

  row.names(fcst) <- NULL

  return(fcst)
}
