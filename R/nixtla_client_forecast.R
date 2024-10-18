#' Sequential version of 'nixtla_client_forecast'
#' This is a private function of 'nixtlar'
#'
#' @param df A data frame with time series data.
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
#'
#' @return 'TimeGPT''s forecast.
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_forecast(df, h=8, id_col="unique_id", level=c(80,95))
#' }
#'
nixtla_client_forecast <- function(df, h=8, freq=NULL, id_col="unique_id", time_col="ds", target_col="y", X_df=NULL, level=NULL, quantiles=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE, add_history=FALSE, model="timegpt-1"){

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

  # Validate input size ----
  if(finetune_steps > 0 | !is.null(level) | add_history){
    num_rows <- df |>
      dplyr::group_by(.data$unique_id) |>
      dplyr::summarise(initial_size = dplyr::n())

    if(any(num_rows$initial_size < model_params$input_size+model_params$horizon)){
      stop(paste0("Your time series is too short. Please make sure that each of your series contains at least ", model_params$input_size+model_params$horizon, " observations."))
    }
  }

  # Make sure there is enough data ----
  if(h > model_params$horizon){
    message("The specified horizon h exceeds the model horizon. This may lead to less accurate forecasts. Please consider using a smaller horizon.")
  }

  # Restrict input if necessary ----
  contains_exogenous <- any(!(names(df) %in% c("unique_id", "ds", "y")))

  if(!contains_exogenous & finetune_steps == 0 & !add_history){
    if(is.null(level) & is.null(quantiles)){
      input_samples = model_params$input_size
    }else{
      input_samples = 3*model_params$input_size+max(model_params$horizon, h)
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

    if(!is.null(X_df)){
      vals_df <- .validate_exogenous(df, h, X_df)

      X_df <- X_df |> # same order as df
        dplyr::select(dplyr::all_of(c("unique_id", "ds", vals_df)))

      future_exogenous <- X_df |>
        dplyr::select(-dplyr::all_of(c("unique_id", "ds"))) |>
        as.list()

      message(paste0("Using future exogenous features: ", paste(names(future_exogenous), collapse=", ")))
      names(future_exogenous) <- NULL
      payload$series$X_future <- future_exogenous
    }
  }

  # Make request ----
  setup <- .get_client_steup()
  req <- httr2::request(paste0(setup$base_url, "v2/forecast")) |>
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
  if(inherits(df_info$last_ds, "character")){
    if(length(df_info$last_ds) > 1){
      dt <- sample(df_info$last_ds, 2)
    }else{
      dt <- df_info$last_ds[1]
    }
    nch <- max(nchar(as.character(dt)))
    if(nch <= 10){
      df_info$dates <- lubridate::ymd(df_info$last_ds)
    }else{
      df_info$dates <- lubridate::ymd_hms(df_info$last_ds)
    }
  }else{
    # assumes df_info$last_ds is already a date-object
    df_info$dates <- df_info$last_ds
  }

  dates_df <- .generate_output_dates(df_info, freq, h)

  dates_long_df <- dates_df |>
    tidyr::pivot_longer(cols = everything(), names_to = "unique_id", values_to = "ds")

  if(inherits(df$unique_id, "integer")){
    dates_long_df$unique_id <- as.numeric(dates_long_df$unique_id)
  }

  dates_long_df <- dates_long_df |>
      dplyr::arrange(.data$unique_id)

  forecast <- cbind(dates_long_df, fc)

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

  # Add fitted values if required ----
  if(add_history){
    fitted <- nixtla_client_historic(df=df, freq=freq, id_col=id_col, time_col=time_col, target_col=target_col, level=level, quantiles=quantiles, finetune_steps=finetune_steps, finetune_loss=finetune_loss, clean_ex_first=clean_ex_first)
    forecast <- dplyr::bind_rows(fitted, forecast)
  }

  return(forecast)
}
