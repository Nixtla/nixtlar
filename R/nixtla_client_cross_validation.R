#' Sequential version of 'nixtla_client_cross_validation'
#' This is a private function of 'nixtlar'
#'
#' @param df A data frame with time series data.
#' @param h Forecast horizon.
#' @param freq Frequency of the data.
#' @param id_col Column that identifies each series.
#' @param time_col Column that identifies each timestep.
#' @param target_col Column that contains the target variable.
#' @param level The confidence levels (0-100) for the prediction intervals.
#' @param quantiles Quantiles to forecast. Should be between 0 and 1.
#' @param n_windows Number of windows to evaluate.
#' @param step_size Step size between each cross validation window. If NULL, it will equal the forecast horizon (h).
#' @param finetune_steps Number of steps used to finetune 'TimeGPT' in the new data.
#' @param finetune_loss Loss function to use for finetuning. Options are: "default", "mae", "mse", "rmse", "mape", and "smape".
#' @param clean_ex_first Clean exogenous signal before making the forecasts using 'TimeGPT'.
#' @param model Model to use, either "timegpt-1" or "timegpt-1-long-horizon". Use "timegpt-1-long-horizon" if you want to forecast more than one seasonal period given the frequency of the data.
#'
#' @return A data frame with 'TimeGPT''s cross validation result.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_cross_validation(df, h = 8, id_col = "unique_id", n_windows = 5)
#' }
#'
nixtla_client_cross_validation <- function(df, h=8, freq=NULL, id_col="unique_id", time_col="ds", target_col="y", level=NULL, quantiles=NULL, n_windows=1, step_size=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, model="timegpt-1"){

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

  # Obtain model parameters ----
  model_params <- .get_model_params(model, freq)

  # Make sure there is enough data ----
  if(h > model_params$horizon){
    message("The specified horizon h exceeds the model horizon. This may lead to less accurate forecasts. Please consider using a smaller horizon.")
  }

  if(finetune_steps > 0 | !is.null(level)){
    num_rows <- df |>
      dplyr::group_by(.data$unique_id) |>
      dplyr::summarise(initial_size = dplyr::n())

    if(any(num_rows$initial_size < model_params$input_size+model_params$horizon)){
      stop(paste0("Your time series is too short. Please make sure that each of your series contains at least ", model_params$input_size+model_params$horizon, " observations."))
    }
  }

  # Define step_size if required ----
  if(is.null(step_size)){
    step_size <- h
  }

  # Restrict input if necessary ----
  contains_exogenous <- any(!(names(df) %in% c("unique_id", "ds", "y")))
  if(!contains_exogenous & finetune_steps == 0){
    # Input is restricted only when there are no exogenous variables and no finetuning
    if(is.null(level) & is.null(quantiles)){
      input_samples = model_params$input_size+h+step_size*(n_windows-1)
    }else{
      input_samples = 3*model_params$input_size+max(model_params$horizon, h)+h+step_size*(n_windows-1)
    }

    num_rows <- df |>
      dplyr::group_by(.data$unique_id) |>
      dplyr::summarise(initial_size = dplyr::n())

    if (any(input_samples > num_rows$initial_size)){
      stop(paste0("Your time series is too short. Please make sure that each of your series contains at least ", model_params$input_size+model_params$horizon, " observations."))
    }

    df <- df |>
      dplyr::group_by(.data$unique_id) |>
      dplyr::slice_tail(n = input_samples) |>
      dplyr::ungroup()
  }

  # Extract unique ids, sizes, and last times ----
  uids <- unique(df$unique_id)

  df_info <- df |>
    dplyr::group_by(.data$unique_id) |>
    dplyr::summarise(
      size = dplyr::n(),
      last_ds = dplyr::nth(.data$ds, -1)
    )

  # Create payload ----
  payload <- list(
    series =  list(
      sizes = as.list(df_info$size),
      y = as.list(df$y)
    ),
    model = model,
    h = h,
    n_windows = n_windows,
    step_size = step_size,
    freq = freq,
    clean_ex_first = clean_ex_first,
    finetune_steps = finetune_steps,
    finetune_loss = finetune_loss
  )

  # Add level or quantiles ----
  if(!is.null(level) && !is.null(quantiles)){
    stop("You should include 'level' or 'quantiles' but not both.")
  }

  if (!is.null(level)) {
    if (any(level < 0 | level > 100)) {
      stop("Level should be between 0 and 100.")
    }
    payload[["level"]] <- as.list(level)
  } else if (!is.null(quantiles)) {
    if (any(quantiles < 0 | quantiles > 1)) {
      stop("Quantiles should be between 0 and 1.")
    }
    lvl <- .level_from_quantiles(quantiles)
    payload[["level"]] <- as.list(lvl$level)
  }

  # Add exogenous variables ----
  if(contains_exogenous){
    exogenous <- df |>
      dplyr::select(-dplyr::all_of(c("unique_id", "ds", "y"))) |>
      as.list()

    message(paste0("Using historical exogenous features: ", paste(names(exogenous), collapse=", ")))
    names(exogenous) <- NULL
    payload$series$X <- exogenous
  }

  # Make request ----
  setup <- .get_client_steup()
  req <- httr2::request(paste0(setup$base_url, "v2/cross_validation")) |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", setup$api_key)
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
  fc <- data.frame(TimeGPT = unlist(resp$mean))

  if("intervals" %in% names(resp) & !is.null(resp$intervals)){
    intervals <- data.frame(lapply(resp$intervals, unlist))
    names(intervals) <- paste0("TimeGPT-", names(resp$intervals))
    fc <- cbind(fc, intervals)
  }

  # Rename quantile columns if present ----
  if(!is.null(quantiles)){
    cols_table <- lvl$ql_df$quantiles_col
    names(cols_table) <- lvl$ql_df$level_col
    names(fc) <- ifelse(names(fc) %in% names(cols_table), cols_table[names(fc)], names(fc))

    # Add 0.5 quantile if present
    if(0.5 %in% quantiles){
      fc <- fc |>
        mutate("TimeGPT-q-50" = .data$TimeGPT)
    }

    fc <- fc |>
      dplyr::select(.data$TimeGPT, tidyselect::starts_with("TimeGPT-q")) |>
      dplyr::select(.data$TimeGPT, sort(tidyselect::peek_vars()))
  }

  # Add unique ids and dates to forecast ----
  ids <- rep(uids, each = h*n_windows)

  idxs <- unlist(resp$idxs)+1 # R indices start at 0

  dates <- df$ds[idxs]
  dates <- as.POSIXct(dates)

  yvals <- df$y[idxs]

  window_starts <- seq(0, sum(unlist(resp$sizes)) - 1, by = h)
  cutoff_idxs <- rep(idxs[window_starts + 1] - 1, each = h)
  cutoff_dates <- df$ds[unique(cutoff_idxs)]
  cutoff <- do.call(c, sapply(cutoff_dates, function(i) rep(i, times = h), simplify = FALSE))
  cutoff <- as.POSIXct(cutoff)

  dt <- data.frame(
    unique_id = ids,
    ds = dates,
    cutoff = cutoff,
    y = yvals
  )

  forecast <- cbind(dt, fc)

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
