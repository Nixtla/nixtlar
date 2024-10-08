#' Sequential version of 'nixtla_client_detect_anomalies'
#' This is a private function of 'nixtlar'
#'
#' @param df A data frame with time series data.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence level (0-100) for the prediction interval used in anomaly detection. Default is 99.
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#'
#' @return A data frame with the anomalies detected in the historical period.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_anomaly_detection(df, id_col="unique_id")
#' }
#'
nixtla_client_detect_anomalies <- function(df, freq=NULL, id_col="unique_id", time_col="ds", target_col="y", level=c(99), clean_ex_first=TRUE, model="timegpt-1"){

  # Validate input ----
  if(!is.data.frame(df) & !inherits(df, "tbl_df") & !inherits(df, "tsibble")){
    stop("Only data frames, tibbles, and tsibbles are allowed.")
  }

  # Rename columns ----
  names(df)[which(names(df) == time_col)] <- "ds"
  names(df)[which(names(df) == target_col)] <- "y"

  cols <- c("ds", "y") %in% names(df)
  if(any(!cols)){
    stop(paste0("The following columns are missing: ", paste(c("ds", "y")[!cols], collapse = ", ")))
  }

  if(is.null(id_col)){
    # create unique_id for single series
    df <- df |>
      dplyr::mutate(unique_id = "ts_0") |>
      dplyr::select(c("unique_id", tidyselect::everything()))
  }else{
    names(df)[which(names(df) == id_col)] <- "unique_id"
  }

  # More input validation ----
  if(any(is.na(df$y))){
    stop(paste0("Target column '", target_col, "' cannot contain missing values."))
  }

  # Infer frequency if necessary ----
  freq <- infer_frequency(df, freq)

  # Extract unique ids, sizes, and last times ----
  uids <- unique(df$unique_id)

  df_info <- df |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::summarise(
      size = dplyr::n(),
      last_ds = dplyr::nth(.data$ds, -1)
    )

  # Select level ----
  if(length(level) > 1){
    message(paste0("Multiple levels are not allowed for anomaly detection. Defaulting to the largest level: ", max(level)))
  }

  # Create payload ----
  payload <- list(
    series =  list(
      sizes = as.list(df_info$size),
      y = as.list(df$y)
    ),
    model = model,
    freq = freq,
    clean_ex_first = clean_ex_first,
    level = max(level)
  )

  # Add exogenous variables ----
  contains_exogenous <- any(!(names(df) %in% c("unique_id", "ds", "y")))
  if(contains_exogenous){
    exogenous <- df |>
      dplyr::select(-dplyr::all_of(c("unique_id", "ds", "y"))) |>
      as.list()

    message(paste0("Using historical exogenous features: ", paste(names(exogenous), collapse=", ")))
    names(exogenous) <- NULL
    payload$series$X <- exogenous
  }

  # Make request ----
  url <- "https://api.nixtla.io/v2/anomaly_detection"
  req <- httr2::request(url) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", .get_api_key())
    ) |>
    httr2::req_user_agent("nixtlar") |>
    httr2::req_body_json(data = payload) |>
    httr2::req_retry(
      max_tries = 6,
      is_transient = .transient_errors
    )

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Extract response ----
  fc <- data.frame(
    anomaly = unlist(resp$anomaly),
    TimeGPT = unlist(resp$mean)
  )

  if("intervals" %in% names(resp) & !is.null(resp$intervals)){
    intervals <- data.frame(lapply(resp$intervals, unlist))
    names(intervals) <- paste0("TimeGPT-", names(resp$intervals))
    fc <- cbind(fc, intervals)
  }

  # Add unique ids and dates to forecast ----
  df_info$fitted_sizes <- unlist(resp$sizes)

  grouped_df_list <- df |>
    dplyr::select(dplyr::all_of(c("unique_id", "ds", "y"))) |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::group_split()

  df_tail <- purrr::map2_dfr(grouped_df_list, unique(df_info$fitted_sizes), ~slice_tail(.x, n = .y))

  nch <- nchar(df_tail$ds[1])
  if(nch <= 10){
    df_tail$ds <- lubridate::ymd(df_tail$ds)
  }else{
    df_tail$ds <- lubridate::ymd_hms(df_tail$ds)
  }

  forecast <- cbind(df_tail, fc)

  # Rename columns back ----
  if(is.null(id_col)){
    forecast <- forecast |>
      dplyr::select(-dplyr::all_of(c("unique_id")))
  }else if(id_col != "unique_id"){
    names(forecast)[which(names(forecast) == "unique_id")] <- id_col
  }

  if(time_col != "ds"){
    names(forecast)[which(names(forecast) == "ds")] <- time_col
  }

  return(forecast)
}
