#' Sequential version of 'nixtla_client_historic'
#' This is a private function of 'nixtlar'
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
#'
#' @return 'TimeGPT''s forecast for the in-sample period.
#' @export
#' @keywords private
#'
#' @examples
#' \dontrun{
#'   nixtlar::nixtla_set_api_key("YOUR_API_KEY")
#'   df <- nixtlar::electricity
#'   fcst <- nixtlar::nixtla_client_historic(df, id_col="unique_id", level=c(80,95))
#' }
#'
.nixtla_client_historic_seq <- function(df, freq=NULL, id_col=NULL, time_col="ds", target_col="y", level=NULL, quantiles=NULL, finetune_steps=0, finetune_loss="default", clean_ex_first=TRUE){

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
    # id_col is not NULL
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
      last_ds = dplyr::nth(ds, -1)
    )

  # Create payload ----
  payload <- list(
    series =  list(
      sizes = as.list(df_info$size),
      y = as.list(df$y)
    ),
    uids = uids,
    last_times = df_info$last_ds,
    freq = freq,
    model = model,
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
  url <- "https://api.nixtla.io/v2/historic_forecast"
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
  fc <- data.frame(TimeGPT = unlist(resp$data$mean))

  if("intervals" %in% names(resp$data) & !is.null(resp$data$intervals)){
    intervals <- data.frame(lapply(resp$data$intervals, unlist))
    names(intervals) <- paste0("TimeGPT-", names(resp$data$intervals))
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
  forecast <- 42

  # Rename columns back ----
  if(id_col != "unique_id"){
    names(forecast)[which(names(forecast) == "unique_id")] <- id_col
  }
  if(time_col != "ds"){
    names(forecast)[which(names(forecast) == "ds")] <- time_col
  }

  return(forecast)
}
