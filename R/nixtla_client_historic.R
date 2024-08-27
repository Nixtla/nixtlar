#' Generate 'TimeGPT' forecast for the in-sample period (historical period).
#'
#' @param df A tsibble or a data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
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
nixtla_client_historic <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=NULL, quantiles=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, model="timegpt-1", num_partitions=NULL){

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
  url_historic <- "https://api.nixtla.io/historic_forecast_multi_series"

  payload_list <- .partition_payload(timegpt_data, num_partitions)

  future::plan(future::multisession)

  responses <- .make_request(url_historic, payload_list)

  # Extract fitted values ----
  fitted_list <- lapply(responses, function(resp) {
    fit_list <- lapply(resp$data$forecast$data, unlist)
    fit <- data.frame(do.call(rbind, fit_list))
    names(fit) <- resp$data$forecast$columns
    return(fit)
  })

  fitted <- do.call(rbind, fitted_list)
  fitted <- fitted[,-which(names(fitted) == "y")]
  fitted[, 3:ncol(fitted)] <- future.apply::future_lapply(fitted[, 3:ncol(fitted)], as.numeric)

  # Rename quantile columns if necessary
  if(!is.null(quantiles)){
    cols_table <- lvl$ql_df$quantiles_col
    names(cols_table) <- lvl$ql_df$level_col
    names(fitted) <- ifelse(names(fitted) %in% names(cols_table), cols_table[names(fitted)], names(fitted))

    fitted <- fitted[, !grepl("hi|lo", names(fitted))]

    # Add 0.5 quantile if present
    if(0.5 %in% quantiles){
      fitted <- fitted |>
        mutate(quantile50 = .data$TimeGPT)
      names(fitted)[which(names(fitted) == "quantile50")] <- "TimeGPT-q-50"
    }

    if(is.null(id_col)){
      fitted <- fitted |>
        dplyr::select(.data$ds, .data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
        dplyr::select(.data$ds, .data$TimeGPT, sort(tidyselect::peek_vars()))
    }else{
      fitted <- fitted |>
        dplyr::select(.data$unique_id, .data$ds, .data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
        dplyr::select(.data$unique_id, .data$ds, .data$TimeGPT, sort(tidyselect::peek_vars()))
    }
  }

  # Date transformation ----
  fitted <- .transform_output_dates(fitted, id_col, "ds", freq, data$flag)

  # Rename columns ----
  names(fitted)[which(names(fitted) == "ds")] <- time_col
  if(!is.null(id_col)){
    names(fitted)[which(names(fitted) == "unique_id")] <- id_col
  }else{
    # remove unique_id column
    fitted <- fitted |>
      dplyr::select(-c(.data$unique_id))
  }

  row.names(fitted) <- NULL

  return(fitted)
}
