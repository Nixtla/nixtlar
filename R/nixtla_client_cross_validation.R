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
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param n_windows Number of windows to evaluate.
#' @param step_size Step size between each cross validation window. If NULL, it will equal the forecast horizon (h).
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
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
nixtla_client_cross_validation <- function(df, h=8, freq=NULL, id_col=NULL, time_col="ds", target_col="y", X_df=NULL, level=NULL, quantiles=NULL, n_windows=1, step_size=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, model="timegpt-1", num_partitions=NULL){

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
  url_cv <- "https://api.nixtla.io/cross_validation_multi_series"

  payload_list <- .partition_payload(timegpt_data, num_partitions)

  future::plan(future::multisession)

  responses <- .make_request(url_cv, payload_list)

  # Extract cross-validation ----
  cross_val_list <- lapply(responses, function(resp) {
    cv_list <- lapply(resp$data$forecast$data, unlist)
    cv <- data.frame(do.call(rbind, cv_list))
    names(cv) <- resp$data$forecast$columns
    return(cv)
  })

  res <- do.call(rbind, cross_val_list)

  res[, 4:ncol(res)] <- future.apply::future_lapply(res[, 4:ncol(res)], as.numeric)

  # Rename quantile columns if necessary
  if(!is.null(quantiles)){
    cols_table <- lvl$ql_df$quantiles_col
    names(cols_table) <- lvl$ql_df$level_col
    names(res) <- ifelse(names(res) %in% names(cols_table), cols_table[names(res)], names(res))

    res <- res[, !grepl("hi|lo", names(res))]

    # Add 0.5 quantile if present
    if(0.5 %in% quantiles){
      res <- res |>
        mutate(quantile50 = .data$TimeGPT)
      names(res)[which(names(res) == "quantile50")] <- "TimeGPT-q-50"
    }

    if(is.null(id_col)){
      res <- res |>
        dplyr::select(.data$ds, .data$cutoff, .data$y, .data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
        dplyr::select(.data$ds, .data$cutoff, .data$y, .data$TimeGPT, sort(tidyselect::peek_vars()))
    }else{
      res <- res |>
        dplyr::select(.data$unique_id, .data$ds, .data$cutoff, .data$y, .data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
        dplyr::select(.data$unique_id, .data$ds, .data$cutoff, .data$y, .data$TimeGPT, sort(tidyselect::peek_vars()))
    }
  }

  # Date transformation ----
  res <- .transform_output_dates(res, id_col, "ds", freq, data$flag)
  new_cutoff <- future.apply::future_lapply(res$cutoff, lubridate::ymd_hms)
  res$cutoff <- do.call(c, new_cutoff)

  # Rename columns ----
  colnames(res)[which(colnames(res) == "ds")] <- time_col
  if(!is.null(id_col)){
    colnames(res)[which(colnames(res) == "unique_id")] <- id_col
  }else{
    # remove unique_id column
    res <- res |>
      dplyr::select(-c(.data$unique_id))
  }

  row.names(res) <- NULL

  return(res)
}
