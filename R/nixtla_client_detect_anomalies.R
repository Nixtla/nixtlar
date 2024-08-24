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
#' @param num_partitions A positive integer, "auto", or NULL specifying the number of partitions. When set to "auto", the number of partitions is equal to the number of available cores. When NULL, it defaults to a single partition.
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
nixtla_client_detect_anomalies <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=c(99), clean_ex_first=TRUE, model="timegpt-1", num_partitions=NULL){

  start <- Sys.time()

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

  # Create request ----
  url_anomaly <- "https://api.nixtla.io/anomaly_detection_multi_series"

  payload_list <- .partition_payload(timegpt_data, num_partitions)

  future::plan(future::multisession)

  responses <- .make_request(url_anomaly, payload_list)

  # Extract anomalies ----
  anomaly_list <- lapply(responses, function(resp) {
    anm_list <- lapply(resp$data$forecast$data, unlist)
    anm <- data.frame(do.call(rbind, anm_list))
    names(anm) <- resp$data$forecast$columns
    return(anm)
  })

  res <- do.call(rbind, anomaly_list)
  res[, 3:ncol(res)] <- future.apply::future_lapply(res[, 3:ncol(res)], as.numeric)

  # Date transformation ----
  res <- .transform_output_dates(res, "ds", freq, data$flag)

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

  end <- Sys.time()
  print(paste0("Total execution time: ", end-start))

  return(res)
}
